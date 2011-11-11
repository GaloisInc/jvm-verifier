TOP=../../..
JAVA_IMPL=../build
SAWSCRIPT=$(TOP)/dist/build/sawScript/sawScript
CLASSES_JAR=$(TOP)/jdk1.6/classes.jar
RUNSAWSCRIPT=$(SAWSCRIPT) -c $(JAVA_IMPL) -j$(CLASSES_JAR)

REF_IMPL_SOURCE := spec/ecc.cry spec/Set.cry spec/Morphism.cry spec/Field.cry

all: make_sbvs verify-individually

make_sbvs: 
	(cd spec/sbv && sh make_sbvs)

PROOFS :=           \
  is_zero.saw       \
  set_zero.saw      \
  set_unit.saw      \
  field_add.saw     \
  field_sub.saw     \
  field_dbl.saw     \
  field_dbl_dec.saw \
  field_mul3.saw    \
  field_mul4.saw    \
  field_mul8.saw    \
  field_mul.saw     \
  field_sq.saw      \
  ec_double.saw     \

# Run SAWScript verfication using the toplevel orchestration script
verify:
	$(RUNSAWSCRIPT) proofs/toplevel.saw

# Run SAWScript verification on each operation individually
verify-individually: 
	@echo ==================== Verification Started ==============================
	@for i in $(PROOFS);\
          do echo === $$i === ;\
             $(RUNSAWSCRIPT) proofs/$$i;\
          done

.PHONY: clean
clean:
	rm -rf spec/sbv/generated 
