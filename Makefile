################################################################################
# Commands relating to Haskell library
################################################################################

.PHONY: build
build:
	@stack build

.PHONY: test
test:
	@stack test

.PHONY: haddock
haddock:
	@stack haddock

.PHONY: format
format:
	@ormolu --mode inplace --cabal-default-extensions $(shell git ls-files '*.hs')

.PHONY: format-check
format-check:
	@ormolu --mode check --cabal-default-extensions $(shell git ls-files '*.hs')

################################################################################
# Commands relating to Haskell Symposium 2021 paper
################################################################################

include doc/doc.mk
