import "rules.saw";
method com.galois.ecc.P384ECC64.ec_twin_mul_aux_f
{
  var t :: int;
  let res = ref_ec_twin_mul_aux_f(t);
  return res;
  verify { rewrite; yices; }; // Also discharges using only abc
};
