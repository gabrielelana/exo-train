.PHONY: run

build-n-run: build run

build: Main.hs
	stack build

run:
	stack run
