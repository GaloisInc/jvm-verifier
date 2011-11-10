method com.galois.ecc.P384ECC64.ec_double 
{
  var args[0]                         :: com.galois.ecc.JacobianPoint;
  var args[0].x, args[0].y, args[0].z :: int[12];
  var this.t1, this.t2                :: int[12];
  const this.field_prime := split(field_prime) : [12][32];

  let vx = join(valueOf(args[0].x));
  let vy = join(valueOf(args[0].y));
  let vz = join(valueOf(args[0].z));
 
  // Let res be the Cryptol struct formed by the result of the Cryptol
  // reference implementation of ec_double.
  let res = ref_ec_double({ x = vx; y = vy; z = vz });

  ensures valueOf(args[0].x) := split(res.x) : [12][32];
  ensures valueOf(args[0].y) := split(res.y) : [12][32];
  ensures valueOf(args[0].z) := split(res.z) : [12][32];

  modifies: valueOf(this.t1), valueOf(this.t2);

  verifyUsing: quickcheck 10;
};
