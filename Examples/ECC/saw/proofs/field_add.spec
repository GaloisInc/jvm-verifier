method com.galois.ecc.P384ECC64.field_add 
{
  var args[0], args[1], args[2] :: int[12];
  mayAlias { args[0], args[1], args[2] };
  const this.field_prime := split(field_prime) : [12][32];

  let jargs1 = join(valueOf(args[1]));
  let jargs2 = join(valueOf(args[2]));
  ensures 
    valueOf(args[0]) := split(ref_field_add(jargs1, jargs2)) : [12][32];

  verifyUsing: abc;
};

