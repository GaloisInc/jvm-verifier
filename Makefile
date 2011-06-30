build:
	cabal configure -fbuild-tests -fbuild-examples -fabc-backend
	cabal build

test: build
	dist/build/Tests/Tests

clean:
	cabal clean

scratch: clean build test
