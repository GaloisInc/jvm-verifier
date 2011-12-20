method com.galois.ecc.P384ECC64.field_dbl
{
  var z, x             :: int[12];
  var this.field_prime :: int[12];
  mayAlias { z, x };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx = join(valueOf(x));
  ensure
    valueOf(z) := split (ref_field_add(jx, jx)) : [12][32];
  verify { rewrite; yices; };
};

