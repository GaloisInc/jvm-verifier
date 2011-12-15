method com.galois.ecc.P384ECC64.ec_twin_mul_init
{
  var r                          :: com.galois.ecc.JacobianPoint;
  var d0, d1                     :: int[12];
  var sPt, s, sMt, t             :: com.galois.ecc.AffinePoint;
  var r.x, r.y, r.z              :: int[12];
  var sPt.x, sPt.y               :: int[12];
  var s.x, s.y                   :: int[12];
  var sMt.x, sMt.y               :: int[12];
  var t.x, t.y                   :: int[12];
  var sPtP, sMtP                 :: com.galois.ecc.JacobianPoint;
  var sPtP.x, sPtP.y, sPtP.z     :: int[12];
  var sMtP.x, sMtP.y, sMtP.z     :: int[12];
  var this.a                     :: int[24];
  var this.h                     :: int[12];
  var this.t1, this.t2, this.t3  :: int[12];
  var this.field_prime           :: int[12];
  var this.field_unit            :: int[12];
  var this.group_order           :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  assert valueOf(this.field_unit)  := split(1 : [384])   : [12][32];
  assert valueOf(this.group_order) := split(group_order) : [12][32];

  let res =
    ref_ec_twin_mul_init( join(valueOf(d0))
                        , { x = join(valueOf(s.x)) ; y = join(valueOf(s.y)) }
                        , join(valueOf(d1))
                        , { x = join(valueOf(t.x)) ; y = join(valueOf(t.y)) }
                        );

  ensure valueOf(r.x) := split(res.r.x) : [12][32];
  ensure valueOf(r.y) := split(res.r.y) : [12][32];
  ensure valueOf(r.z) := split(res.r.z) : [12][32];

  modify valueOf(d0);
  modify valueOf(d1);

  modify valueOf(sPt.x);
  modify valueOf(sPt.y);
  modify valueOf(sMt.x);
  modify valueOf(sMt.y);

  modify valueOf(sPtP.x);
  modify valueOf(sPtP.y);
  modify valueOf(sPtP.z);
  modify valueOf(sMtP.x);
  modify valueOf(sMtP.y);
  modify valueOf(sMtP.z);

  return res.special;

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify valueOf(this.h);
  modify valueOf(this.field_prime);
  modify valueOf(this.field_unit);
  modify valueOf(this.group_order);

  modify valueOf(s.x);
  modify valueOf(s.y);
  modify valueOf(t.x);
  modify valueOf(t.y);
  //quickcheck 1;
  //quickcheck 15000;
  verify { rewrite; yices; /* smtlib; */};
};
