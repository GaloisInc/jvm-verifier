method com.galois.ecc.P384ECC64.field_mul8
{
  var args[0], args[1] :: int[12];
  mayAlias { args[0], args[1] };

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let vx = join(valueOf(args[1]));
  let x2 = ref_field_add(vx, vx);
  let x4 = ref_field_add(x2, x2);

  ensure 
    valueOf(args[0]) := split (ref_field_add(x4, x4)) : [12][32];

  verify yices;
};
