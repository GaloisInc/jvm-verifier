method com.galois.ecc.P384ECC64.ec_twin_mul_aux2
{
  var c0, c1, e0, e1, shift, d0i, d1i           :: int;
  var dv1, dv2                                  :: boolean;
  var this.aux2Rslt                             :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.u0                          :: int;
  var this.aux2Rslt.u1                          :: int;
  var this.aux2Rslt.c0p                         :: int;
  var this.aux2Rslt.c1p                         :: int;
  var this.aux2Rslt.e0p                         :: int;
  var this.aux2Rslt.e1p                         :: int;
  var this.aux2Rslt.shp                         :: int;

  assert (shift >=s 0:[32]) && (shift <=s 31:[32]);
  assert (dv1 >=u 0:[32]) && (dv1 <=u 1:[32]); // dv1 is a boolean
  assert (dv2 >=u 0:[32]) && (dv2 <=u 1:[32]); // dv2 is a boolean

  let res = ref_ec_twin_mul_aux2(c0, c1, e0, e1, shift, d0i, d1i, dv1, dv2);

  ensure this.aux2Rslt.u0  := res.tma2_u0;
  ensure this.aux2Rslt.u1  := res.tma2_u1;
  ensure this.aux2Rslt.c0p := res.tma2_c0';
  ensure this.aux2Rslt.c1p := res.tma2_c1';
  ensure this.aux2Rslt.e0p := res.tma2_e0';  
  ensure this.aux2Rslt.e1p := res.tma2_e1';  
  ensure this.aux2Rslt.shp := res.tma2_sh';  

  verify { abc; };
};
