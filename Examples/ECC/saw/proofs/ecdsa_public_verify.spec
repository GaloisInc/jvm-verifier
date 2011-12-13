method com.galois.ecc.P384ECC64.verifySignature
{
  var args[0]                         :: int[12];
  var args[1]                         :: com.galois.ecc.Signature;
  var args[2]                         :: com.galois.ecc.PublicKey;
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[1].r, args[1].s            :: int[12];
  var args[2].x, args[2].y            :: int[12];
  var this.qPoint                     :: com.galois.ecc.AffinePoint;
  var this.qPoint.x                   :: int[12];
  var this.qPoint.y                   :: int[12];
  var this.basePoint                  :: com.galois.ecc.AffinePoint;
  var this.basePoint.x                :: int[12];
  var this.basePoint.y                :: int[12];
  var this.field_prime                :: int[12];
  var this.group_order                :: int[12];
  var this.field_unit                 :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  assert valueOf(this.field_unit)  := split(1 : [384])   : [12][32];
  assert this.width := 12 : [32];

  let b1 = basePoint;
  assert valueOf(this.basePoint.x)  := split(b1.x)        : [12][32];
  assert valueOf(this.basePoint.y)  := split(b1.y)        : [12][32];

  let e = join(valueOf(args[0]));
  let r = join(valueOf(args[1].r));
  let s = join(valueOf(args[1].s));
  let qx = join(valueOf(args[2].x));
  let qy = join(valueOf(args[2].y));

  return ref_ecdsa_public_verify(e, r, s, {x = qx; y = qy});

  modify valueOf(this.a), valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  /*
  modify valueOf(this.qPoint.y), valueOf(this.q.x);
  modify valueOf(this.basePoint.y);
  modify valueOf(this.basePoint.x);
  modify valueOf(args[2].x);
  modify valueOf(args[2].y);
  modify valueOf(args[1].r);
  modify valueOf(args[1].s);
  modify valueOf(this.group_order);
  modify valueOf(this.field_prime);
  modify valueOf(this.field_unit);
  modify valueOf(args[0]);
  */
  //quickcheck 1;
  verify { rewrite; /*yices; smtlib;*/ };
};
