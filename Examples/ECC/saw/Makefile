# NOTE: (TEMPORARY) The relative paths are from the point of view of the
# "proofs" directory.
#
# This is to work around a bug in sawScript, where it seems # to be
# having trouble finding file that are not in the current directory.

TOP=../../../..
JAVA_IMPL=../../build

SAWSCRIPT:=$(TOP)/dist/build/sawScript/sawScript
CLASSES_JAR:=$(TOP)/jdk1.6/classes.jar
RUNSAWSCRIPT:=$(SAWSCRIPT) -c $(JAVA_IMPL) -j$(CLASSES_JAR) +RTS -K64M -RTS

REF_IMPL_SOURCE := spec/ecc.cry spec/Set.cry spec/Morphism.cry spec/Field.cry

all: make_java make_sbvs verify

make_java:
	(cd .. && make build)

make_sbvs: 
	(cd spec/sbv && sh make_sbvs)

PROOFS :=              \
  add.saw              \
  assign.saw           \
  decFieldPrime.saw    \
  ecdsa_sign.saw       \
  ec_double.saw        \
  ec_full_add.saw      \
  ec_full_sub.saw      \
  ec_mul.saw           \
  ec_mul_aux.saw       \
  ec_mul_init.saw      \
  field_add.saw        \
  field_dbl.saw        \
  field_dbl_dec.saw    \
  field_mul.saw        \
  field_mul3.saw       \
  field_mul4.saw       \
  field_mul8.saw       \
  field_red_aux.saw    \
  field_red.saw        \
  field_sq.saw         \
  field_sub.saw        \
  group_add.saw        \
  incFieldPrime.saw    \
  is_zero.saw          \
  leq.saw              \
  mod_div.saw          \
  mod_half.saw         \
  mod_sub.saw          \
  mul.saw              \
  set_unit.saw         \
  set_zero.saw         \
  shr.saw              \
  sq.saw               \
  sub.saw              \

.PHONY: clean verify

# Run SAWScript verification on each operation individually
verify:
	@echo ==================== Verification Started ==============================
	@cd proofs; \
         for i in $(PROOFS);\
          do echo === $$i === ;\
             $(RUNSAWSCRIPT) $$i;\
          done

%.test: proofs/%.saw
	cd proofs; $(RUNSAWSCRIPT) $*.saw

clean:
	rm -rf spec/sbv/generated 
