import "rules.saw";
method com.galois.ecc.P384ECC64.ec_twin_mul_aux2
{
  var c0, c1           :: int;
  var this.aux2Rslt    :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.h0 :: int;
  var this.aux2Rslt.h1 :: int;

  assert (c0 >=s 0:[32]) && (c0 <=s 2:[32]);
  assert (c1 >=s 0:[32]) && (c1 <=s 2:[32]);

  let res = ref_ec_twin_mul_aux2(c0, c1);

  ensure this.aux2Rslt.h0 := res.tma2_h0;
  ensure this.aux2Rslt.h1 := res.tma2_h1;

  quickcheck 100;
//  verify { rewrite; yices; /*smtlib;*/ };
};
