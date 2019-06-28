{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Data.Int
import           Data.Word
import           Data.Bits
import qualified Data.Vector.Storable           as SV
import qualified Data.Vector.Storable.Mutable   as SVM

-- import           Data.Time.Clock
-- import qualified Control.DeepSeq
-- import qualified System.Environment

import           Criterion
import           Criterion.Main

import           Control.LVish                  as LVishSched
import qualified Control.Par.Class              as PC
import           Control.Par.MergeSort.Internal
import           Control.Par.ST
import qualified Control.Par.ST.StorableVec2    as V

-- TODO: Add vector-algortihms benchmarks.

{-
main :: IO ()
main =
  do
    putStrLn $ "warming up"
    warmup 0 5 input
    putStrLn $ "running experiments"
    putStrLn ""
    runMany 0 10 input
  where
    input = SV.map hash $ SV.fromList $ [0 .. (10^(8 :: Int32) - 1)]

runMany :: Int -> Int -> SV.Vector Int32 -> IO ()
runMany i n input =
  if i == n then return () else do
    putStrLn $ "==== run " ++ show i ++ " ===="
    t0 <- getCurrentTime
    let result = sortPV 10000 10000 CSort HSMerge input
    putStrLn $ (show $ result SV.! 0) ++ " " ++ (show $ result SV.! 42)
    t1 <- getCurrentTime
    putStrLn $ "wall " ++ (show $ diffUTCTime t1 t0)
    putStrLn ""
    runMany (i+1) n input

warmup :: Int -> Int -> SV.Vector Int32 -> IO ()
warmup i n input =
  if i == n then return () else do
    t0 <- getCurrentTime
    let result = sortPV 10000 10000 CSort HSMerge input
    putStrLn $ (show $ result SV.! 0) ++ " " ++ (show $ result SV.! 42)
    t1 <- getCurrentTime
    putStrLn $ (show $ diffUTCTime t1 t0)
    warmup (i+1) n input
-}

main :: IO ()
main = do
  -- args <- System.Environment.getArgs
  -- let size = read $ args !! 0
  let size = 10^(6 :: Int32)
  putStrLn $ "size " ++ show size
  putStrLn ""
  defaultMain
    [ env (return $ SV.map hash $ SV.fromList $ [0 .. size-1]) $ \(vec :: SV.Vector Int32) ->
        bgroup "sorting benchmarks" $ concat $
        flip map [CSort] $ \ssMeth ->
        flip map [HSMerge] $ \smMeth ->
        -- TODO(osa): Thresholds should be varying?
        mkBench 10000 10000 ssMeth smMeth vec
    ]

mkBench :: Int -> Int -> SSort -> SMerge -> SV.Vector Int32 -> Benchmark
mkBench ssThres smThres ssMeth smMeth vec =
    bench msg $ nf (sortPV ssThres smThres ssMeth smMeth) vec
  where
    msg = "sort" ++ " " ++ show ssThres
                 ++ " " ++ show smThres
                 ++ " " ++ show ssMeth
                 ++ " " ++ show smMeth

-- | Out of place sort on immutable vectors.
--
-- TODO(osa): Copied from tests, maybe copy to a shared place.
--
sortPV :: Int -> Int -> SSort -> SMerge -> SV.Vector Int32 -> SV.Vector Int32
sortPV ssThres smThres ssMeth smMeth vec =
    -- TODO(osa): Maybe remove copying here by just taking mutable vec and
    -- returning mutable one.
    LVishSched.runPar $ V.runParVec2T (0, SV.length vec) $ do
      vec' <- liftST $ SV.thaw vec
      sortPV' ssThres smThres ssMeth smMeth vec' >> do
        (rawL, _) <- V.reify
        sv <- liftST $ SV.freeze rawL
        return $ sv

-- | Sort the vector in the left component of the state.
--
-- TODO(osa): Like sortPV, copied.
sortPV' :: (PC.ParMonad p, PC.ParThreadSafe p, PC.ParIVar p, PC.FutContents p (),
            PC.ParFuture p, HasPut e, HasGet e) =>
           Int -> Int -> SSort -> SMerge ->
           SVM.STVector s1 Int32 -> V.ParVec2T s1 Int32 Int32 p e s ()
sortPV' ssThres smThres ssMeth smMeth vec = do
--    (_, right) <- V.reify
--    SS.put (STTup2 (SFlp vec) (SFlp right))
    V.installL vec
    mergeSort_int32 ssThres smThres ssMeth smMeth

hashWord :: Word64 -> Word64
hashWord w = v8 where
  v1 = w * 3935559000370003845 + 2691343689449507681
  v2 = v1 `xor` (v1 `shiftR` 21)
  v3 = v2 `xor` (v2 `shiftL` 37)
  v4 = v3 `xor` (v3 `shiftR` 4)
  v5 = v4 * 4768777513237032717
  v6 = v5 `xor` (v5 `shiftL` 20)
  v7 = v6 `xor` (v6 `shiftR` 41)
  v8 = v7 `xor` (v7 `shiftL` 5)

hash :: Int32 -> Int32
hash x = fromIntegral (hashed `mod` bound) where
  bound = fromIntegral (maxBound :: Int32)
  hashed = hashWord (fromIntegral x)
