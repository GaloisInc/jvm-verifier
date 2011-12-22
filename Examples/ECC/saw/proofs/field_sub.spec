method com.galois.ecc.P384ECC64.field_sub 
{
  var z, x, y          :: int[12];
  var this.field_prime :: int[12];
  mayAlias { z, x, y };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx = join(valueOf(x));
  let jy = join(valueOf(y));
  ensure valueOf(z) := split(ref_field_sub(jx, jy)) : [12][32];
  verify { rewrite; yices; };
};
