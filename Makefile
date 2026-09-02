build: ## Build the library
	@cabal build all

test: ## Run the test suite
	@cabal test all --test-show-details=direct

lint: ## Run hlint
	@hlint src test

format: ## Format the sources and the cabal file
	@fourmolu --quiet --mode inplace src test
	@cabal-fmt --inplace json-pointer.cabal

format-check: ## Check that the sources and the cabal file are formatted
	@fourmolu --quiet --mode check src test
	@cabal-fmt --check json-pointer.cabal

check: format-check lint test ## Run all the checks

clean: ## Remove the cabal build artifacts
	@cabal clean

repl: ## Start a cabal REPL
	@cabal repl lib:json-pointer

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

SHELL := /usr/bin/env bash

# Spelled out rather than $(MAKECMDGOALS), so that the prerequisites of `check`
# are phony too - `test` would otherwise be shadowed by the test directory.
.PHONY: build test lint format format-check check clean repl help

.DEFAULT_GOAL := help
