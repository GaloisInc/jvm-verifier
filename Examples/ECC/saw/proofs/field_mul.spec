method com.galois.ecc.P384ECC64.field_mul 
{
  var args[0], args[1], args[2] :: int[12];
  var this.a                    :: int[24];
  mayAlias { args[0], args[1], args[2] };

  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let vx = join(valueOf(args[1]));
  let vy = join(valueOf(args[2]));

  ensure valueOf(args[0]) := split(ref_field_mul(vx, vy)) : [12][32];
  modify valueOf(this.a);
  verify rewrite;
};
