method com.galois.ecc.P384ECC64.field_sq {
  var z, x             :: int[12];
  var this.a           :: int[24];
  var this.field_prime :: int[12];
  mayAlias { z, x };
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  ensure valueOf(z) := split(ref_field_sq(join(valueOf(x)))) : [12][32];
  modify valueOf(this.a);
  verify rewrite;
};
