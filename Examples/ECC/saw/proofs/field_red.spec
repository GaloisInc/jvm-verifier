method com.galois.ecc.P384ECC64.field_red
{
  var args[0] :: int[12];
  var args[1] :: int[24];
  const this.field_prime := split(field_prime) : [12][32];
  
  let va = join(valueOf(args[1]));

  ensures 
    valueOf(args[0]) := split(ref_field_mod(va)) : [12][32];

  verifyUsing: quickcheck 100;
};
