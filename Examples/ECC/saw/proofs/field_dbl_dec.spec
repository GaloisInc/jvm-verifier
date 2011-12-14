method com.galois.ecc.P384ECC64.field_dbl_dec
{
  var args[0], args[1] :: int[12];

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let vz = join(valueOf(args[0]));
  let vx = join(valueOf(args[1]));

  ensure 
    valueOf(args[0]) :=
      split (ref_field_sub(ref_field_sub(vz, vx), vx)) : [12][32];

  verify rewrite;
};

