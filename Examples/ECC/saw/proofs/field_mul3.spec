method com.galois.ecc.P384ECC64.field_mul3
{
  var z, x             :: int[12];
  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jx = join(valueOf(x));
  ensure 
    valueOf(z) := 
      split (ref_field_add(jx, ref_field_add(jx, jx))) : [12][32];
  verify rewrite;
};
