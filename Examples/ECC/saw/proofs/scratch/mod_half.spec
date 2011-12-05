enable ref_mod_half;
method com.galois.ecc.P384ECC64.mod_half
{
  var x, p :: int[12];
  mayAlias { x, p };

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let jargs1 = join(valueOf(x));
  let jargs2 = join(valueOf(p));
  ensure valueOf(x) := split(ref_mod_half(jargs2, jargs1)) : [12][32];

  verify { abc; };
};
