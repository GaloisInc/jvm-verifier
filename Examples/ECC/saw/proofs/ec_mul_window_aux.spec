method com.galois.ecc.P384ECC64.ec_mul_window_aux
{
  var j                         :: int;
  var r                         :: com.galois.ecc.JacobianPoint;
  var hi, kai, kip1             :: int;
  var hi2, kai2, ki2p1          :: int;
  var s, s3, s5                 :: com.galois.ecc.AffinePoint;
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];
  var s3.x, s3.y                :: int[12];
  var s5.x, s5.y                :: int[12];
  var this.field_prime          :: int[12];

  //assert j >=u 0:[32] && j <u 384:[32];                      // 0 <= j < 384

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_window_aux( { h = 0:[384]
                                   ; j = j
                                   ; r = { x = join(valueOf(r.x))
                                         ; y = join(valueOf(r.y))
                                         ; z = join(valueOf(r.z))
                                         }
                                   }
                                 , hi, kai, kip1
                                 , hi2, kai2, ki2p1
                                 , { x = join(valueOf(s.x))
                                   ; y = join(valueOf(s.y))
                                   }
                                 , { x = join(valueOf(s3.x))
                                   ; y = join(valueOf(s3.y))
                                   }
                                 , { x = join(valueOf(s5.x))
                                   ; y = join(valueOf(s5.y))
                                   } );
  ensure valueOf(r.x) := split(res.r.x) : [12][32];
  //ensure valueOf(r.y) := split(res.r.y) : [12][32];
  //ensure valueOf(r.z) := split(res.r.z) : [12][32];

  return res.j;
  modify valueOf(r.x);
  modify valueOf(r.y);
  modify valueOf(r.z);
  modify valueOf(s.x);
  modify valueOf(s.y);
  modify valueOf(s3.x);
  modify valueOf(s3.y);
  modify valueOf(s5.x);
  modify valueOf(s5.y);
  modify valueOf(this.a), valueOf(this.h);
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);

  modify valueOf(this.field_prime);

  verify { rewrite; /*yices;*/ };
};
