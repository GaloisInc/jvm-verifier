method com.galois.ecc.P384ECC64.mul_joel_inner
{
  var a       :: int[4]; // 24
  var ij      :: int;
  var xi, yj  :: int;
  var d       :: long;

  assert (ij >=s 0 :[32]) && (ij <=s 3:[32]); //23:[32]);

  let ja  = join(valueOf(a)) : [128];
  let res = ref_mul_joel_inner(ja, ij, xi, yj, d);

  ensure valueOf(a) := split(res.mji_a) : [4][32];
  return res.mji_d;

//  verify { rewrite; yices; };
  verify {abc;} ;
};
