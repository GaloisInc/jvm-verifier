method com.galois.ecc.P384ECC64.mul_inner
{
  var azero   :: boolean;
  var a       :: int[24];
  var ij      :: int;
  var xi, yj  :: int;
  var d       :: long;

  assert (ij >=s 0 :[32]) && (ij <=s 23:[32]);

  let ja  = join(valueOf(a)) : [768];
  let res = ref_mul_java_inner(azero, ja, ij, xi, yj, d);

  ensure valueOf(a) := split(res.mji_a) : [24][32];
  return res.mji_d;

//  verify { rewrite; yices; };
  verify {abc;} ;
};
