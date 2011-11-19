method com.galois.ecc.P384ECC64.ec_mul
{
  var args[0]                           :: com.galois.ecc.JacobianPoint; // Out-parameter multiplication result
  var args[1]                           :: int[12];                      // Scalar multiplier
  var args[2]                           :: com.galois.ecc.AffinePoint;   // Multiplicand point
  var this.h, this.t1, this.t2, this.t3 :: int[12];                      // Scratch
  var this.a                            :: int[24];                      // Modified by ec_mul_aux -> ec_double -> field_mul
  var args[0].x, args[0].y, args[0].z   :: int[12];                      // Jacobian point members
  var args[2].x, args[2].y              :: int[12];                      // Affine point members

  let  d = join(valueOf(args[1]));
  let jx = join(valueOf(args[0].x));
  let jy = join(valueOf(args[0].y));
  let jz = join(valueOf(args[0].z));
  let ax = join(valueOf(args[2].x));
  let ay = join(valueOf(args[2].y));

  const this.field_prime := split(field_prime) : [12][32];

  let r = ref_ec_mul(d, {x = ax; y = ay});

  ensures valueOf(args[0].x) := split(r.x) : [12][32];
  ensures valueOf(args[0].y) := split(r.y) : [12][32];
  ensures valueOf(args[0].z) := split(r.z) : [12][32];

  modifies: valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3), valueOf(this.a);
  verifyUsing: quickcheck 1;
};
