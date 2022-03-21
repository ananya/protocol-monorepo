CABAL ?= cabal --builddir=dist
TEST_TARGET ?= test-suite

default: build

.PHONY: default

prepare:
	$(CABAL) v2-update

.PHONY: prepare

build:
	$(CABAL) v2-build all

clean:
	$(CABAL) v2-clean

.PHONY: build clean

test-suite:
	$(CABAL) v2-test superfluid-protocol-spec-validator

test-demo:
	$(CABAL) v2-exec -- superfluid-protocol-spec-validator --demo

test: $(TEST_TARGET)

dev:
	nodemon -e hs,cabal -i dist -i dist-newstyle -x "make build test || exit 1"

.PHONY: test-suite test-demo test dev

cloc:
	@echo '**** Core Source Files ****'
	cloc packages/core/src
	@echo '**** Non-Core Source Files ****'
	cloc packages/simple/src packages/validator/src
	@echo '**** Test Files ****'
	cloc packages/*/test

.PHONY: loc
