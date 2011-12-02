import "rules.saw";
enable ref_ec_mul_aux;
method com.galois.ecc.P384ECC64.ec_mul_merge_aux 
{
  var args[0]                         :: com.galois.ecc.JacobianPoint;
  var args[1]                         :: com.galois.ecc.AffinePoint;
  var args[2], args[3], args[4]       :: int;
  var this.a                          :: int[24];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[0].x, args[0].y, args[0].z :: int[12];
  var args[1].x, args[1].y            :: int[12];

/*
  assume ref_is_field(join(valueOf(args[0].x)));
  assume ref_is_field(join(valueOf(args[0].y)));
  assume ref_is_field(join(valueOf(args[0].z)));
  assume ref_is_field(join(valueOf(args[1].x)));
  assume ref_is_field(join(valueOf(args[1].y)));
*/

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_aux( { x = join(valueOf(args[0].x))
                            ; y = join(valueOf(args[0].y))
                            ; z = join(valueOf(args[0].z))
                            }
                          , { x = join(valueOf(args[1].x))
                            ; y = join(valueOf(args[1].y))
                            }
                          , args[2]
                          , args[3]
                          , args[4]
                          );
  ensure valueOf(args[0].x) := split(res.x) : [12][32];
  ensure valueOf(args[0].y) := split(res.y) : [12][32];
  ensure valueOf(args[0].z) := split(res.z) : [12][32];
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

