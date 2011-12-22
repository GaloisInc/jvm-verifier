method com.galois.ecc.P384ECC64.verifySignature
{
  var hashValue                 :: int[12];
  var signature                 :: com.galois.ecc.Signature;
  var publicKey                 :: com.galois.ecc.PublicKey;
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var this.u1, this.u2          :: int[12];
  var signature.r, signature.s  :: int[12];
  var publicKey.x, publicKey.y  :: int[12];
  var this.qPoint               :: com.galois.ecc.AffinePoint;
  var this.qPoint.x             :: int[12];
  var this.qPoint.y             :: int[12];
  var this.rP                   :: com.galois.ecc.JacobianPoint;
  var this.rP.x                 :: int[12];
  var this.rP.y                 :: int[12];
  var this.rP.z                 :: int[12];
  var this.sPtP                 :: com.galois.ecc.JacobianPoint;
  var this.sPtP.x               :: int[12];
  var this.sPtP.y               :: int[12];
  var this.sPtP.z               :: int[12];
  var this.sMtP                 :: com.galois.ecc.JacobianPoint;
  var this.sMtP.x               :: int[12];
  var this.sMtP.y               :: int[12];
  var this.sMtP.z               :: int[12];
  var this.sPt                  :: com.galois.ecc.AffinePoint;
  var this.sPt.x                :: int[12];
  var this.sPt.y                :: int[12];
  var this.sMt                  :: com.galois.ecc.AffinePoint;
  var this.sMt.x                :: int[12];
  var this.sMt.y                :: int[12];
  var this.aux2Rslt             :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.u0          :: int;
  var this.aux2Rslt.u1          :: int;
  var this.aux2Rslt.c0p         :: int;
  var this.aux2Rslt.c1p         :: int;
  var this.aux2Rslt.e0p         :: int;
  var this.aux2Rslt.e1p         :: int;
  var this.aux2Rslt.shp         :: int;
  var this.basePoint            :: com.galois.ecc.AffinePoint;
  var this.basePoint.x          :: int[12];
  var this.basePoint.y          :: int[12];
  var this.field_prime          :: int[12];
  var this.group_order          :: int[12];
  var this.field_unit           :: int[12];

  let zero = split(0 : [384]) : [12][32];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  assert valueOf(this.field_unit)  := split(1 : [384])   : [12][32];
  assert this.width := 12 : [32];
  assert valueOf(this.basePoint.x)  := split(basePoint.x) : [12][32];
  assert valueOf(this.basePoint.y)  := split(basePoint.y) : [12][32];
  assert valueOf(this.sPt.x) := zero;
  assert valueOf(this.sPt.y) := zero;
  assert valueOf(this.sMt.x) := zero;
  assert valueOf(this.sMt.y) := zero;

  let e  = join(valueOf(hashValue));
  let r  = join(valueOf(signature.r));
  let s  = join(valueOf(signature.s));
  let qx = join(valueOf(publicKey.x));
  let qy = join(valueOf(publicKey.y));

  return ref_ecdsa_public_verify(e, r, s, {x = qx; y = qy});

  modify this.aux2Rslt.u0, this.aux2Rslt.u1, this.aux2Rslt.c0p, this.aux2Rslt.c1p, 
         this.aux2Rslt.e0p, this.aux2Rslt.e1p, this.aux2Rslt.shp;
  modify valueOf(this.a);
  modify valueOf(this.qPoint.y), valueOf(this.qPoint.x);
  modify valueOf(this.basePoint.y);
  modify valueOf(this.basePoint.x);
  modify valueOf(hashValue);

  ensure valueOf(this.h)      := split(0 : [384]) : [12][32];
  ensure valueOf(this.t1)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.t2)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.t3)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.u1)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.u2)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.x)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.y)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.z)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.x) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.y) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.z) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.x) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.y) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.z) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPt.x)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPt.y)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMt.x)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMt.y)  := split(0 : [384]) : [12][32];

  verify { rewrite; yices; };
};
