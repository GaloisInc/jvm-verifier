method com.galois.ecc.P384ECC64.field_dbl
{
  var args[0], args[1] :: int[12];
  mayAlias { args[0], args[1] };

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let vx = join(valueOf(args[1]));

  ensure
    valueOf(args[0]) := split (ref_field_add(vx, vx)) : [12][32];

  verify abc;
};
