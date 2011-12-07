method com.galois.ecc.P384ECC64.ec_full_sub
{
  var args[0]                         :: com.galois.ecc.JacobianPoint; 
  var args[1]                         :: com.galois.ecc.AffinePoint;
  var this.a                          :: int[24];                      // field_mul (via ec_full_add) modify field 'a'
  var this.t1, this.t2, this.t3       :: int[12];                      // ec_full_add modify these fields
  var args[0].x, args[0].y, args[0].z :: int[12];                      // Jacobian point members
  var args[1].x, args[1].y            :: int[12];                      // Affine point members

  let jx = join(valueOf(args[0].x));
  let jy = join(valueOf(args[0].y));
  let jz = join(valueOf(args[0].z));
  let ax = join(valueOf(args[1].x));
  let ay = join(valueOf(args[1].y));

/*
  assume(ref_is_field(jx));
  assume(ref_is_field(jy));
  assume(ref_is_field(jz));    
  assume(ref_is_field(ax));    
  assume(ref_is_field(ay));    
*/

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let r = ref_ec_full_sub({x = jx; y = jy; z = jz}, {x = ax; y = ay});

  ensure valueOf(args[0].x) := split(r.x) : [12][32];
  ensure valueOf(args[0].y) := split(r.y) : [12][32];
  ensure valueOf(args[0].z) := split(r.z) : [12][32];

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  verify { rewrite; yices; };
};
