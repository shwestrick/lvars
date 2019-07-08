.PHONY: run-mergesort

all: run-mergesort

run-mergesort:
	stack install --local-bin-path ./bin/ par-mergesort:exe:run-mergesort
