import "rules.saw";
method com.galois.ecc.P384ECC64.ec_twin_mul_aux2
{
  var c0, c1, e0, e1, shift :: int;
  var this.aux2Rslt         :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.u0      :: int;
  var this.aux2Rslt.u1      :: int;
  var this.aux2Rslt.c0p     :: int;
  var this.aux2Rslt.c1p     :: int;

  assert (shift >=s 0:[32]) && (shift <=s 31:[32]);

  let res = ref_ec_twin_mul_aux2(c0, c1, e0, e1, shift);

  ensure this.aux2Rslt.u0  := res.tma2_u0;
  ensure this.aux2Rslt.u1  := res.tma2_u1;
  ensure this.aux2Rslt.c0p := res.tma2_c0';
  ensure this.aux2Rslt.c1p := res.tma2_c1';

  verify { abc; };
};
