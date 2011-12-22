method com.galois.ecc.P384ECC64.ec_double 
{
  var r                 :: com.galois.ecc.JacobianPoint;
  var r.x, r.y, r.z     :: int[12];
  var this.t1, this.t2  :: int[12]; // ec_double changes t1 and t2
  var this.a            :: int[24]; // field_mul changes a
  var this.field_prime  :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_double({ x = join(valueOf(r.x))
                          ; y = join(valueOf(r.y))
                          ; z = join(valueOf(r.z))
                          });

  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];

  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.a);

  verify rewrite;
};
disable ref_ec_double;
