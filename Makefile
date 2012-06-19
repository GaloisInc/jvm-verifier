ABC=../../abcBridge
SMTLIB=../../smtLib

build:
	cabal-dev install . ../Verinf $(ABC) $(SMTLIB) --flags='build-tests build-jvmgraph'

test: build
	dist/build/Tests/Tests

clean:
	cabal clean

scratch: clean build test
