method com.galois.ecc.P384ECC64.ec_mul_window_finish
{
  var j, hi, ki                 :: int;
  var r                         :: com.galois.ecc.JacobianPoint;
  var s                         :: com.galois.ecc.AffinePoint;
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];
  var this.field_prime          :: int[12];

  assert j >=u 0:[32] && j <u 384:[32];                      // 0 <= j < 384

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_window_finish(j,
                                     { x = join(valueOf(r.x))
                                     ; y = join(valueOf(r.y))
                                     ; z = join(valueOf(r.z))
                                     },
                                     hi,
                                     ki,
                                     { x = join(valueOf(s.x))
                                     ; y = join(valueOf(s.y))
                                     });
  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];

  modify valueOf(s.x);
  modify valueOf(s.y);
  modify valueOf(this.a), valueOf(this.h);
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify valueOf(this.field_prime);

  verify { rewrite; yices; };
};
