import "rules.saw";
enable ref_ec_mul_aux;
method com.galois.ecc.P384ECC64.ec_mul_merge_aux 
{
  var r                         :: com.galois.ecc.JacobianPoint;
  var s                         :: com.galois.ecc.AffinePoint;
  var m, hi, ki                 :: int;
  var this.a                    :: int[24];
  var this.t1, this.t2, this.t3 :: int[12];
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];

/*
  assume ref_is_field(join(valueOf(r.x)));
  assume ref_is_field(join(valueOf(r.y)));
  assume ref_is_field(join(valueOf(r.z)));
  assume ref_is_field(join(valueOf(s.x)));
  assume ref_is_field(join(valueOf(s.y)));
*/

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_aux( { x = join(valueOf(r.x))
                            ; y = join(valueOf(r.y))
                            ; z = join(valueOf(r.z))
                            }
                          , { x = join(valueOf(s.x))
                            ; y = join(valueOf(s.y))
                            }
                          , m
                          , hi
                          , ki
                          );
  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];
  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);

  verify {
    disable imp_true_elim1;
    rewrite;
    enable imp_true_elim1;
    rewrite;
    // yices; 
    // quickcheck 1;
  };
};

