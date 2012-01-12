method com.galois.ecc.P384ECC64.sq_inner2
{
  var a       :: int[24];
  var ij      :: int;
  var xati    :: int;
  var c       :: long;

  assert (ij >=s 0 :[32]) && (ij <=s 23:[32]);

  let ja  = join(valueOf(a)) : [768];
  let res = ref_sq_java_inner2(ja, ij, xati, c);

  ensure valueOf(a) := split(res.mji_a) : [24][32];
  return res.mji_d;

  verify abc;
};
