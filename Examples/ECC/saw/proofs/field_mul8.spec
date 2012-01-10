method com.galois.ecc.P384ECC64.field_mul8
{
  var z, x             :: int[12];
  var this.field_prime :: int[12];
  mayAlias { z, x };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx = join(valueOf(x));
  let x2 = ref_field_add(jx, jx);
  let x4 = ref_field_add(x2, x2);
  ensure 
    valueOf(z) := split (ref_field_add(x4, x4)) : [12][32];
  verify rewrite;
};
