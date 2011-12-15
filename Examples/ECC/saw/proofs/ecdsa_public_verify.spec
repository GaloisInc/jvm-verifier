method com.galois.ecc.P384ECC64.verifySignature
{
  var args[0]                         :: int[12];
  var args[1]                         :: com.galois.ecc.Signature;
  var args[2]                         :: com.galois.ecc.PublicKey;
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var this.u1, this.u2                :: int[12];
  var args[1].r, args[1].s            :: int[12];
  var args[2].x, args[2].y            :: int[12];
  var this.qPoint                     :: com.galois.ecc.AffinePoint;
  var this.qPoint.x                   :: int[12];
  var this.qPoint.y                   :: int[12];
  var this.rP                         :: com.galois.ecc.JacobianPoint;
  var this.rP.x                       :: int[12];
  var this.rP.y                       :: int[12];
  var this.rP.z                       :: int[12];
  var this.sPtP                       :: com.galois.ecc.JacobianPoint;
  var this.sPtP.x                     :: int[12];
  var this.sPtP.y                     :: int[12];
  var this.sPtP.z                     :: int[12];
  var this.sMtP                       :: com.galois.ecc.JacobianPoint;
  var this.sMtP.x                     :: int[12];
  var this.sMtP.y                     :: int[12];
  var this.sMtP.z                     :: int[12];
  var this.sPt                        :: com.galois.ecc.AffinePoint;
  var this.sPt.x                      :: int[12];
  var this.sPt.y                      :: int[12];
  var this.sMt                        :: com.galois.ecc.AffinePoint;
  var this.sMt.x                      :: int[12];
  var this.sMt.y                      :: int[12];
  var this.aux2Rslt                   :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.u0                :: int;
  var this.aux2Rslt.u1                :: int;
  var this.aux2Rslt.c0p               :: int;
  var this.aux2Rslt.c1p               :: int;
  var this.aux2Rslt.e0p               :: int;
  var this.aux2Rslt.e1p               :: int;
  var this.aux2Rslt.shp               :: int;
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

  assert valueOf(this.basePoint.x)  := split(basePoint.x) : [12][32];
  assert valueOf(this.basePoint.y)  := split(basePoint.y) : [12][32];

  let e = join(valueOf(args[0]));
  let r = join(valueOf(args[1].r));
  let s = join(valueOf(args[1].s));
  let qx = join(valueOf(args[2].x));
  let qy = join(valueOf(args[2].y));

  return ref_ecdsa_public_verify(e, r, s, {x = qx; y = qy});

  modify this.aux2Rslt.u0, this.aux2Rslt.u1, this.aux2Rslt.c0p, this.aux2Rslt.c1p, 
         this.aux2Rslt.e0p, this.aux2Rslt.e1p, this.aux2Rslt.shp;
  modify valueOf(this.a), valueOf(this.h);
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify valueOf(this.u1), valueOf(this.u2);
  modify valueOf(this.qPoint.y), valueOf(this.qPoint.x);
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
  modify valueOf(this.rP.x);
  modify valueOf(this.rP.y);
  modify valueOf(this.rP.z);
  modify valueOf(this.sPtP.x);
  modify valueOf(this.sPtP.y);
  modify valueOf(this.sPtP.z);
  modify valueOf(this.sMtP.x);
  modify valueOf(this.sMtP.y);
  modify valueOf(this.sMtP.z);
  modify valueOf(this.sPt.x);
  modify valueOf(this.sPt.y);
  modify valueOf(this.sMt.x);
  modify valueOf(this.sMt.y);
  verify { rewrite; yices; };
};
