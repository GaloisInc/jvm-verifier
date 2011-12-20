method com.galois.ecc.P384ECC64.field_mul4
{
  var z, x             :: int[12];
  var this.field_prime :: int[12];
  mayAlias { z, x };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx  = join(valueOf(x));
  let dbl = ref_field_add(jx, jx);
  ensure 
    valueOf(z) := split (ref_field_add(dbl, dbl)) : [12][32];
  verify rewrite;
};
