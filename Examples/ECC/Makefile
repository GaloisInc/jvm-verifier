SAWSCRIPT=../../dist/build/sawScript/sawScript
CLASSES_JAR=../../jdk1.6/classes.jar
RUNSAWSCRIPT=$(SAWSCRIPT) -c build -j$(CLASSES_JAR)

build :
	mkdir -p build
	javac -g:vars -cp src:../../support src/com/galois/ecc/*.java -d build
	javac -g:vars -cp build:tests tests/com/galois/ecc/*.java -d build

# Build java doc documentation.
doc :
	javadoc -classpath src -d doc -public src/com/galois/ecc/*

clean :
	rm -rf doc

# Run Java with 32bit data.
run32 :
	java -cp build -d32 com.galois.ecc.ECC

# Run Java with 64bit data (much faster).
run64 :
	java -cp build -d64 com.galois.ecc.ECC

# Run SAWScript verification.
verify: 
	@(cd saw && make verify)

# Run old SAWScript verification script (i.e., the one bundled with
# beta-rc3).
verify-old:
	$(SAWSCRIPT) -c build -j$(CLASSES_JAR) saw/proofs-old.saw

.PHONY : build clean doc run32 run64 verify


