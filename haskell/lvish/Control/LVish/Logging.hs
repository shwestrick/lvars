{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

{-|

Thread-safe Logging with bonus controlled-schedule debugging capabilities.

This module supports logging to memory, serializing messages and deferring the work
of actually printing them.  Another thread can flush the logged messages at its
leisure.

The second capability of this infrastructure is to use the debugging print messages
as points at which to gate the execution of the program.  That is, each `logStrLn_`
call becomes a place where the program blocks and checks in with a central
coordinator, which only allows one thread to unblock at a time.  Thus, if there are
sufficient debug logging messages in the program, this can enable a form of
deterministic replay (and quickcheck-style testing of different interleavings).

 -}

module Control.LVish.Logging
       (
         -- * Log to a shared, global log
         printLog, printLogThread,
         logStrLn_, logLnAt_,

         -- * Global variables
         globalLog, dbgLvl
       )
       where

import qualified Control.Exception as E
import qualified Control.Concurrent.Async as A
import           Data.IORef
import           Data.List (sortBy)
import           GHC.Conc hiding (yield)
import           Control.Concurrent
import           System.IO.Unsafe (unsafePerformIO)
import           System.IO (stderr)
import           System.Environment(getEnvironment)
import           Text.Printf (hPrintf)

import Control.LVish.Types
import qualified Control.LVish.SchedIdempotentInternal as Sched

----------------------------------------------------------------------------------------------------

-- | A Logger coordinates a set of threads that print debug logging messages.
--
--   This are abstract objects supporting only the operations provided by this module
--   and the non-hidden fields of the Logger.
data Logger = Logger { coordinator :: A.Async () -- ThreadId
                                      -- ^ (private) The thread that chooses which action to unblock next
                                      -- and handles printing to the screen as well.
                     , checkPoint :: SmplChan Writer -- ^ The serialized queue of writers attempting to log dbg messages.
                     , logged :: IORef [LogMsg] -- ^ (private) The actual log of messages.
                     , closeIt :: IO () -- ^ (public) A method to complete flushing, close down the helper thread,
                                        -- and generally wrap up.
                     , waitWorkers :: WaitMode  
                     }

-- | A single thread attempting to log a message.  It only unblocks when the attached
-- MVar is filled.
data Writer = Writer { who :: String
                     , continue :: MVar ()
                     , lvl :: Int -- ^ Verbosity-level for the printed message.  
                     , msg :: LogMsg
                       -- TODO: Indicate whether this writer has useful work to do or
                       -- is about to block... this provides a simple notion of
                       -- priority.
                     }

-- | Several different ways we know to wait for quiescence in the concurrent mutator
-- before proceeding.
data WaitMode = WaitTids [ThreadId] -- ^ Wait until a certain set of threads is blocked before proceeding.
              | WaitDynamic -- ^ UNFINISHED: Dynamically track tasks/workers.  The
                            -- num workers starts at 1 and then is modified
                            -- with `incrTasks` and `decrTasks`.
              | WaitNum Int -- ^ UNFINISHED: A fixed set of threads must check-in each round before proceeding.

-- | We allow logging in O(1) time in String or ByteString format.  In practice the
-- distinction is not that important, because only *thunks* should be logged; the
-- thread printing the logs should deal with forcing those thunks.
data LogMsg = StrMsg String 

toString (StrMsg s) = s

-- | Create a new logger, which includes forking a coordinator thread.
--   Takes as argument the number of worker threads participating in the computation.
newLogger :: WaitMode -> IO Logger
newLogger waitWorkers = do
  logged <- newIORef []
--  checkPoint <- newChan
  checkPoint  <- newSmplChan
  coordinator <- A.async $ do
    -- Proceed in rounds, gather the set of actions that may happen in parallel, then
    -- pick one.  We log the series of decisions we make for reproducability.
    let schedloop !num !waiting = do
          -- PROBLEM: how do we detect quiescence?  How do we know how many threads
          -- should check-in?
          --
          -- HEURISTIC: require that we be initialized with a number of worker
          -- threads.  ALL workers are expected to check-in in some form each round.
          let keepWaiting = do yield; schedloop num waiting
              waitMore    = do w <- readSmplChan checkPoint
                               yield
                               schedloop (num+1) (w:waiting)
          case waitWorkers of
            WaitNum target | num >= target -> pickAndProceed waiting
                           | otherwise     -> waitMore
            WaitTids ls -> do
              b <- checkTids ls
              case b of
                True  -> do ls <- flushChan waiting
                            pickAndProceed ls
                False -> keepWaiting

        -- When all threads are quiescent, we can flush the remaining messagers from
        -- the channel to get the whole set of waiting tasks.
        flushChan !acc = do
          x <- tryReadSmplChan checkPoint
          case x of
            Just h  -> flushChan (h:acc)
            Nothing -> return acc

        -- Take the set of logically-in-parallel tasks, choose one, execute it, and
        -- then return to the main scheduler loop.
        pickAndProceed [] = chatter " [Logger] No active tasks, shutting down."
        pickAndProceed waiting = do
          putStr (show (length waiting) ++" ") -- TEMP
          let order a b =
                let s1 = toString (msg a)
                    s2 = toString (msg b) in
                case compare s1 s2 of
                  GT -> GT
                  LT -> LT
                  EQ -> error $" [Logger] Need in-parallel log messages to have an ordering, got two equal:\n "++s1
              sorted = sortBy order waiting
              hd:rst = sorted
          unblockTask hd
          -- Return to the scheduler to wait for the next quiescent point.
          schedloop (length rst) rst

        unblockTask Writer{who,continue,lvl,msg} = do
          -- Print out the message.
          hPrintf stderr (toString msg)
          -- Signal that the thread may continue.
          putMVar continue ()
          
        -- Check whether the worker threads are all quiesced 
        checkTids [] = return True
        checkTids (tid:rst) = do 
          st <- threadStatus tid
          case st of
            ThreadRunning   -> return False
            ThreadFinished  -> checkTids rst
            ThreadBlocked _ -> checkTids rst
            ThreadDied      -> checkTids rst -- Should this be an error condition!?
    return ()
  let closeIt = A.cancel coordinator
  return $! Logger { coordinator, logged, checkPoint, closeIt, waitWorkers }

chatter :: String -> IO ()
chatter = hPrintf stderr 

-- UNFINISHED:
incrTasks = undefined
decrTasks = undefined

-- | Write a log message from the current thread.  
logOn :: Logger -> LogMsg -> IO ()
logOn = undefined

----------------------------------------------------------------------------------------------------
-- Simple channels: we need non-blocking reads so we can't use
-- Control.Concurrent.Chan.  We could use TChan, but I don't want to bring STM into
-- it right now.

-- type MyChan a = Chan a

-- -- | A simple channel.  Take-before-put is the protocol.
-- type SmplChan a = MVar [a]

-- | Simple channels that don't support real blocking.
type SmplChan a = IORef [a]

newSmplChan = newIORef []

-- | Non-blocking read.
tryReadSmplChan ch = do
  ls <- readIORef ch
  x <- atomicModifyIORef' ch $ \ ls -> 
       case ls of
         []  -> ([], Nothing)
         h:t -> (t, Just h)
  return x

-- | Blocking OR busy-waiting read.
readSmplChan ch = do
  x <- tryReadSmplChan ch
  case x of
    Nothing -> do yield; readSmplChan ch
    Just h  -> return h

----------------------------------------------------------------------------------------------------

-- | A target for global log messages.
globalLogger :: Logger
globalLogger = unsafePerformIO $ newLogger (WaitNum numCapabilities)
{-# NOINLINE globalLogger #-}


-- | A global log for global log messages.
globalLog :: IORef [String]
globalLog = unsafePerformIO $ newIORef []
{-# NOINLINE globalLog #-}

----------------------------------------------------------------------------------------------------

-- | The global coordinator that all threads check in with before proceeding.

-- | Atomically add a line to the given log.
logStrLn_ :: String -> IO ()
logLnAt_ :: Int -> String -> IO ()
#ifdef DEBUG_LVAR
#warning "Compiling in LVish DEBUG mode."
logStrLn_ s = logLnAt_ 1 s
logLnAt_ lvl s | dbgLvl >= 5   = putStrLn s
               | dbgLvl >= lvl = atomicModifyIORef globalLog $ \ss -> (s:ss, ())
               | otherwise     = return ()
#else 
logStrLn _  = return ()
logStrLn_ _ = return ()
logLnAt_ _ _ = return ()
{-# INLINE logStrLn #-}
{-# INLINE logStrLn_ #-}
#endif

-- | Print all accumulated log lines.
printLog :: IO ()
printLog = do
  -- Clear the log when we read it:
  lines <- atomicModifyIORef globalLog $ \ss -> ([], ss)
  mapM_ putStrLn $ reverse lines  

-- | The idea here is that while a runPar is underway, we periodically flush out the
-- debug messages.
printLogThread :: IO (IO ())
printLogThread = do
  tid <- forkIO $
         E.catch loop (\ (e :: E.AsyncException) -> do
                        -- One last time on kill:
                        printLog
                        putStrLn " [dbg-log-printer] Shutting down."
                      )
  return (do killThread tid
             let wait = do
                   stat <- threadStatus tid
                   case stat of
                     ThreadRunning -> threadDelay 1000 >> wait
                     _             -> return ()
             wait)
 where
   loop = do
     -- Flush the log at 5Hz:
     printLog
     threadDelay (200 * 1000)
     loop

{-# NOINLINE theEnv #-}
theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

-- | Debugging flag shared by several modules.
--   This is activated by setting the environment variable @DEBUG=1..5@.
-- 
--   By convention @DEBUG=100@ turns on full sequentialization of the program and
--   control over the interleavings in concurrent code, enabling systematic debugging
--   of concurrency problems.
dbgLvl :: Int
#ifdef DEBUG_LVAR
{-# NOINLINE dbgLvl #-}
dbgLvl = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         case reads s of
           ((n,_):_) -> trace (" [!] LVish responding to env Var: DEBUG="++show n) n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s
#else 
{-# INLINE dbgLvl #-}
dbgLvl = 0
#endif

defaultDbg :: Int
defaultDbg = 0

replayDbg :: Int
replayDbg = 100
