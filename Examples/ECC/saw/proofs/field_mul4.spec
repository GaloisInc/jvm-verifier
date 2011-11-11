method com.galois.ecc.P384ECC64.field_mul4
{
  var args[0], args[1] :: int[12];
  mayAlias { args[0], args[1] };
  const this.field_prime := split(field_prime) : [12][32];

  let vx  = join(valueOf(args[1]));
  let dbl = ref_field_add(vx, vx);

  ensures 
    valueOf(args[0]) := split (ref_field_add(dbl, dbl)) : [12][32];
 
  verifyUsing: abc;    
};
