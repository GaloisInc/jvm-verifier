method com.galois.ecc.P384ECC64.field_dbl_dec
{
  var z, x             :: int[12];
  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let jz = join(valueOf(z));
  let jx = join(valueOf(x));
  ensure 
    valueOf(z) :=
      split (ref_field_sub(ref_field_sub(jz, jx), jx)) : [12][32];
  verify rewrite;
};

