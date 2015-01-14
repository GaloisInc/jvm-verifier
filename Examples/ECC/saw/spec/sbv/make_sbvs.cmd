# This file creates a subdirectory sbv and stores the sbvs needed for
# ecc verification.
#
# SBV Files are created for the following types:
# * Reference versions of field operations that are split over 32-bit words.
# * Reference versions of field operations
# * EC double routine that uses field operations split over 32-bit words with
#   uninterpreted field operations.
# * EC double routine that uses field operations split over 384-bit integers 
#   with uninterpreted field operations.
#

:set sbv
:set +v

:load ../ecc.cry
:fm mul_java_inner     "generated/mul_java_inner.sbv"
:fm sq_java_inner1     "generated/sq_java_inner1.sbv"
:fm sq_java_inner2     "generated/sq_java_inner2.sbv"
:fm aset_384           "generated/aset_384.sbv"
:fm aset_768           "generated/aset_768.sbv"
:fm adc_384            "generated/adc_384.sbv"
:fm sbb_384            "generated/sbb_384.sbv"

:load sbv_uninterpret_mul_inner.cry
:fm group_red_aux_java "generated/group_red_aux_java.sbv"
:fm group_mul_aux_java "generated/group_mul_aux_java.sbv"
:fm sq_java_loop       "generated/sq_java_loop.sbv"

:load sbv_uninterpret_sq_loop.cry
:fm p384_safe_product  "generated/p384_safe_product.sbv"
:fm mul_java           "generated/mul_java.sbv"
:fm sq_java            "generated/sq_java.sbv"
:fm p384_mod_add       "generated/p384_mod_add.sbv"
:fm p384_mod_sub       "generated/p384_mod_sub.sbv"
:fm p384_mod_half      "generated/p384_mod_half.sbv"
:fm p384_incFieldPrime "generated/p384_incFieldPrime.sbv"
:fm p384_decFieldPrime "generated/p384_decFieldPrime.sbv"
:fm p384_field_mod     "generated/p384_field_mod.sbv"
:fm p384_group_add     "generated/p384_group_add.sbv"
:fm p384_group_red     "generated/p384_group_red.sbv"
:load sbv_uninterpret_group_red.cry
:set sbv_strictWords=True
:fm p384_group_mul     "generated/p384_group_mul.sbv"
:set sbv_strictWords=False

:load sbv_uninterpret_field_mod.cry
:fm p384_is_field_val "generated/p384_is_field_val.sbv"
:fm p384_field_add    "generated/p384_field_add.sbv"
:fm p384_field_sub    "generated/p384_field_sub.sbv"
:fm p384_field_neg    "generated/p384_field_neg.sbv"
:fm p384_field_mul    "generated/p384_field_mul.sbv"
:fm p384_field_sq     "generated/p384_field_sq.sbv"

:load sbv_uninterpret_field_ops.cry
:fm p384_jacobify    "generated/p384_ec_jacobify.sbv"
:fm p384_affinify    "generated/p384_ec_affinify.sbv"
:fm p384_ec_double   "generated/p384_ec_double.sbv"

:load sbv_uninterpret_ec_double.cry
:fm p384_ec_full_add "generated/p384_ec_full_add.sbv"

:load sbv_uninterpret_ec_full_add.cry
:fm p384_ec_full_sub "generated/p384_ec_full_sub.sbv"

:load sbv_uninterpret_ec_point_ops.cry
:fm p384_ec_mul_init "generated/p384_ec_mul_init.sbv"
:fm p384_ec_mul_aux "generated/p384_ec_mul_aux.sbv"
:fm p384_ec_mul_window_aux "generated/p384_ec_mul_window_aux.sbv"
:fm p384_ec_mul_window_finish "generated/p384_ec_mul_window_finish.sbv"

:load sbv_uninterpret_ec_mul_aux.cry
:set sbv_strictWords=True
:fm p384_ec_mul "generated/p384_ec_mul.sbv"
:fm p384_ec_mul_window "generated/p384_ec_mul_window.sbv"

:load sbv_uninterpret_ec_mul.cry
:fm p384_ec_twin_mul_aux_F32 "generated/p384_ec_twin_mul_aux_F.sbv"
:fm p384_ec_twin_mul_aux1    "generated/p384_ec_twin_mul_aux1.sbv"
:fm p384_ec_twin_mul_aux2    "generated/p384_ec_twin_mul_aux2.sbv"
:fm p384_ec_twin_mul_init    "generated/p384_ec_twin_mul_init.sbv"

:load sbv_uninterpret_ec_twin_mul_auxes.cry
:fm p384_ec_twin_mul "generated/p384_ec_twin_mul.sbv"

:load sbv_uninterpret_ec_twin_mul.cry
:fm p384_ecdsa_sign "generated/p384_ecdsa_sign.sbv"
:fm p384_ecdsa_public_verify "generated/p384_ecdsa_public_verify.sbv"

:quit