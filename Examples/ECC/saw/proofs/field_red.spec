method com.galois.ecc.P384ECC64.field_red
{
  var z, this.field_prime :: int[12];
  var a                   :: int[24];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let a' = join(valueOf(a));
  ensure 
    valueOf(z) := split(ref_field_mod(a')) : [12][32];

  //quickcheck 100000;
  verify { abc; };
};