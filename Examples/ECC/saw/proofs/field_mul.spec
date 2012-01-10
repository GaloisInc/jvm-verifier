method com.galois.ecc.P384ECC64.field_mul 
{
  var z, x, y          :: int[12];
  var this.a           :: int[24];
  var this.field_prime :: int[12];
  mayAlias { z, x, y };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx = join(valueOf(x));
  let jy = join(valueOf(y));
  ensure valueOf(z) := split(ref_field_mul(jx, jy)) : [12][32];
  modify valueOf(this.a);
  verify rewrite;
};
