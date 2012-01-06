method com.galois.ecc.P384ECC64.mul_joel_inner
{
  var a              :: int[24];
  var j, ij          :: int;
  var xi, yj, d, aij :: long;

  assert (j >=s 0:[32]) && (j <=s 11:[32]);
  assert (ij >=s 0 :[32]) && (ij <=s 23:[32]);

  let ja  = join(valueOf(a)) : [768];
  let res = ref_mul_joel_inner(ja, j, ij, xi, yj, d, aij);

  ensure valueOf(a) := split(res.mji_a) : [24][32];
  return res.mji_d;

//  verify { rewrite; yices; };
  verify {abc;} ;
};
