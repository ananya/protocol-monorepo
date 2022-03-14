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

loc:
	@echo '**** Core Source Files ****'
	find core/src -name '*.hs' | xargs wc -l
	@echo '**** Non-Core Source Files ****'
	find simple/src validator/src -name '*.hs' | xargs wc -l
	@echo '**** Test Files ****'
	find */test -name '*.hs' | xargs wc -l

.PHONY: loc
