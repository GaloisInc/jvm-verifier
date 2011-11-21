TOP=../../..
JAVA_IMPL=../build
SAWSCRIPT=$(TOP)/dist/build/sawScript/sawScript
CLASSES_JAR=$(TOP)/jdk1.6/classes.jar
RUNSAWSCRIPT=$(SAWSCRIPT) -c $(JAVA_IMPL) -j$(CLASSES_JAR) +RTS -K64M -RTS

REF_IMPL_SOURCE := spec/ecc.cry spec/Set.cry spec/Morphism.cry spec/Field.cry

all: make_sbvs verify-individually

make_sbvs: 
	(cd spec/sbv && sh make_sbvs)

PROOFS :=              \
  is_zero.saw          \
  set_zero.saw         \
  set_unit.saw         \
  field_add.saw        \
  field_sub.saw        \
  incFieldPrime.saw    \
  decFieldPrime.saw    \
  field_dbl.saw        \
  field_dbl_dec.saw    \
  field_mul3.saw       \
  field_mul4.saw       \
  field_mul8.saw       \
  field_mul.saw        \
  field_sq.saw         \
  field_red.saw        \
  ec_double.saw        \
  ec_full_add.saw      \
  ec_full_sub.saw      \
  ec_mul_merge_aux.saw \

.PHONY: clean verify

# Run SAWScript verification on each operation individually
verify:
	@echo ==================== Verification Started ==============================
	@for i in $(PROOFS);\
          do echo === $$i === ;\
             $(RUNSAWSCRIPT) proofs/$$i;\
          done

%.test: proofs/%.saw
	$(RUNSAWSCRIPT) $<

clean:
	rm -rf spec/sbv/generated 
