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
  let zero = split(0 : [384]) : [12][32];
  assert valueOf(sPt.x) := zero;
  assert valueOf(sPt.y) := zero;
  assert valueOf(sMt.x) := zero;
  assert valueOf(sMt.y) := zero;
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
  let special = join(valueOf(s.x)) == join(valueOf(t.x));

  ensure valueOf(sPt.x) := split(res.sPt.x) : [12][32];
    //if special then zero else (split(res.sPt.x) : [12][32]);
  ensure valueOf(sPt.y) := split(res.sPt.y) : [12][32];
    //if special then zero else (split(res.sPt.y) : [12][32]);
  ensure valueOf(sMt.x) := split(res.sMt.x) : [12][32];
    //if special then zero else (split(res.sMt.x) : [12][32]);
  ensure valueOf(sMt.y) := split(res.sMt.y) : [12][32];
    //if special then zero else (split(res.sMt.y) : [12][32]);

  return if special then 1 : [32] else 0 : [32];

  modify valueOf(sPtP.x);
  modify valueOf(sPtP.y);
  modify valueOf(sPtP.z);
  modify valueOf(sMtP.x);
  modify valueOf(sMtP.y);
  modify valueOf(sMtP.z);

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify valueOf(this.h);

  verify { rewrite; yices; };
};
