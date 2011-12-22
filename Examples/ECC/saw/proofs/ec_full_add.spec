method com.galois.ecc.P384ECC64.ec_full_add
{
  var r                         :: com.galois.ecc.JacobianPoint; 
  var t                         :: com.galois.ecc.AffinePoint;
  var this.a                    :: int[24]; // field_mul modifies field 'a'
  var this.t1, this.t2, this.t3 :: int[12]; // ec_full_add modifies these fields
  var r.x, r.y, r.z             :: int[12]; // Jacobian point members
  var t.x, t.y                  :: int[12]; // Affine point members
  var this.field_prime          :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_full_add( { x = join(valueOf(r.x))
                             ; y = join(valueOf(r.y))
                             ; z = join(valueOf(r.z))
                             }, 
                             { x = join(valueOf(t.x))
                             ; y = join(valueOf(t.y))
                             }
                           );

  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  verify { rewrite; yices; };
};
