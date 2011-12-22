method com.galois.ecc.P384ECC64.ec_mul_init
{
  var r                         :: com.galois.ecc.JacobianPoint;
  var d                         :: int[12];
  var s                         :: com.galois.ecc.AffinePoint;
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];
  var this.field_prime          :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_init( join(valueOf(d)), 
                            { x = join(valueOf(s.x))
                            ; y = join(valueOf(s.y))
                            }
                           );

  ensure valueOf(r.x) := split(res.r.x) : [12][32];
  ensure valueOf(r.y) := split(res.r.y) : [12][32];
  ensure valueOf(r.z) := split(res.r.z) : [12][32];
  ensure valueOf(this.h) := split(res.h) : [12][32];

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  verify abc;
};
