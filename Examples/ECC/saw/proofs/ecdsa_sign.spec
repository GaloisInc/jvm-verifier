method com.galois.ecc.P384ECC64.signHash
{
  var args[0]                         :: com.galois.ecc.Signature;
  var args[1]                         :: int[12];
  var args[2]                         :: int[12];
  var args[3]                         :: int[12];
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[0].r, args[0].s            :: int[12];

  var this.width                      :: int;
  var this.field_prime                :: int[12];
  var this.field_unit                 :: int[12];
  var this.group_order                :: int[12];
  var this.rP                         :: com.galois.ecc.JacobianPoint;
  var this.rP.x                       :: int[12];
  var this.rP.y                       :: int[12];
  var this.rP.z                       :: int[12];
  var this.basePoint                  :: com.galois.ecc.AffinePoint;
  var this.basePoint.x                :: int[12];
  var this.basePoint.y                :: int[12];
  var this.basePoint3                 :: com.galois.ecc.AffinePoint;
  var this.basePoint3.x               :: int[12];
  var this.basePoint3.y               :: int[12];
  var this.basePoint5                 :: com.galois.ecc.AffinePoint;
  var this.basePoint5.x               :: int[12];
  var this.basePoint5.y               :: int[12];

  let b1 = basePoint;
  let b1j = ref_ec_jacobify(b1);
  let b4 = ref_ec_double(ref_ec_double(b1j));
  let b3j = ref_ec_full_sub(b4, b1);
  let b5j = ref_ec_full_add(b4, b1);
  let b3 = ref_ec_affinify(b3j);
  let b5 = ref_ec_affinify(b5j);
  assert valueOf(this.basePoint.x)  := split(b1.x)        : [12][32];
  assert valueOf(this.basePoint.y)  := split(b1.y)        : [12][32];
  assert valueOf(this.basePoint3.x) := split(b3.x)        : [12][32];
  assert valueOf(this.basePoint3.y) := split(b3.y)        : [12][32];
  assert valueOf(this.basePoint5.x) := split(b5.x)        : [12][32];
  assert valueOf(this.basePoint5.y) := split(b5.y)        : [12][32];
  assert valueOf(this.group_order)  := split(group_order) : [12][32];
  assert valueOf(this.field_prime)  := split(field_prime) : [12][32];
  assert valueOf(this.field_unit)   := split(1:[384])     : [12][32];
  assert this.width                 := 12                 : [32];

  let d = join(valueOf(args[1]));
  let e = join(valueOf(args[2]));
  let k = join(valueOf(args[3]));
  assert d != 0:[384];
  assert k != 0:[384];
  assert d <u group_order;
  assert e <u group_order;
  assert k <u group_order;

  let res = ref_ecdsa_sign(d, e, k);

  ensure valueOf(args[0].r) := split(res.r) : [12][32];
  ensure valueOf(args[0].s) := split(res.s) : [12][32];
  return res.r == 0:[384] || res.s == 0:[384];

  modify valueOf(this.a), valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  //quickcheck 1;
  verify { rewrite; /*yices; smtlib;*/ };
};
