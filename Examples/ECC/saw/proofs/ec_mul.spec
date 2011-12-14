method com.galois.ecc.P384ECC64.ec_mul
{
  var args[0]                         :: com.galois.ecc.JacobianPoint;
  var args[1]                         :: int[12];
  var args[2]                         :: com.galois.ecc.AffinePoint;
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[0].x, args[0].y, args[0].z :: int[12];
  var args[2].x, args[2].y            :: int[12];

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let d  = join(valueOf(args[1]));
  let sx = join(valueOf(args[2].x));
  let sy = join(valueOf(args[2].y));

  let res = ref_ec_mul(d, { x = sx; y = sy });

  ensure valueOf(args[0].x) := split(res.x) : [12][32];
  ensure valueOf(args[0].y) := split(res.y) : [12][32];
  ensure valueOf(args[0].z) := split(res.z) : [12][32];
  modify valueOf(this.a), valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  //quickcheck 1;
  verify { rewrite; /*yices; smtlib;*/ };
};
