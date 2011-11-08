TOP=../../..
IMPL=..
SAWSCRIPT=$(TOP)/dist/build/sawScript/sawScript
CLASSES_JAR=$(TOP)/jdk1.6/classes.jar
RUNSAWSCRIPT=$(SAWSCRIPT) -c $(IMPL)/build -j$(CLASSES_JAR)

PROOFS :=                \
  P384ECC64.is_zero.saw  \
  P384ECC64.set_zero.saw

# Run SAWScript verification.
verify: 
	@for i in $(PROOFS);\
          do echo === $$i === ;\
             $(RUNSAWSCRIPT) proofs/$$i;\
          done

