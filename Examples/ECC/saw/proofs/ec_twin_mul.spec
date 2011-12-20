method com.galois.ecc.P384ECC64.ec_twin_mul
{
  var r                         :: com.galois.ecc.JacobianPoint;
  var d0                        :: int[12];
  var s                         :: com.galois.ecc.AffinePoint;
  var d1                        :: int[12];
  var t                         :: com.galois.ecc.AffinePoint;
  var sPtP                      :: com.galois.ecc.JacobianPoint;
  var sMtP                      :: com.galois.ecc.JacobianPoint;
  var sPt                       :: com.galois.ecc.AffinePoint;
  var sMt                       :: com.galois.ecc.AffinePoint;
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var this.aux2Rslt             :: com.galois.ecc.TwinMulAux2Rslt;
  var this.aux2Rslt.u0          :: int;
  var this.aux2Rslt.u1          :: int;
  var this.aux2Rslt.c0p         :: int;
  var this.aux2Rslt.c1p         :: int;
  var this.aux2Rslt.e0p         :: int;
  var this.aux2Rslt.e1p         :: int;
  var this.aux2Rslt.shp         :: int;
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];
  var t.x, t.y                  :: int[12];
  var sPtP.x, sPtP.y, sPtP.z    :: int[12];
  var sMtP.x, sMtP.y, sMtP.z    :: int[12];
  var sPt.x, sPt.y              :: int[12];
  var sMt.x, sMt.y              :: int[12];
  var this.field_prime          :: int[12];
  var this.group_order          :: int[12];
  var this.field_unit           :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  assert valueOf(this.field_unit)  := split(1 : [384]) : [12][32];
  let zero = split(0 : [384]) : [12][32];
  assert valueOf(sPt.x) := zero;
  assert valueOf(sPt.y) := zero;
  assert valueOf(sMt.x) := zero;
  assert valueOf(sMt.y) := zero;

  let d0' = join(valueOf(d0));
  let sx  = join(valueOf(s.x));
  let sy  = join(valueOf(s.y));
  let d1' = join(valueOf(d1));
  let tx  = join(valueOf(t.x));
  let ty  = join(valueOf(t.y));

  let res = ref_ec_twin_mul(d0', { x = sx; y = sy },
                            d1', { x = tx; y = ty });

  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];
  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];
  modify valueOf(this.a);
  modify valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify this.aux2Rslt.u0, this.aux2Rslt.u1,
         this.aux2Rslt.c0p, this.aux2Rslt.c1p,
         this.aux2Rslt.e0p, this.aux2Rslt.e1p, this.aux2Rslt.shp;

  modify valueOf(sPtP.x);
  modify valueOf(sPtP.y);
  modify valueOf(sPtP.z);
  modify valueOf(sMtP.x);
  modify valueOf(sMtP.y);
  modify valueOf(sMtP.z);
  modify valueOf(sPt.x);
  modify valueOf(sPt.y);
  modify valueOf(sMt.x);
  modify valueOf(sMt.y);
  modify valueOf(t.x);
  modify valueOf(t.y);
  modify valueOf(s.x);
  modify valueOf(s.y);

  verify { rewrite; };
};
