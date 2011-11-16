
method com.galois.ecc.P384ECC64.incFieldPrime 
{
  var args[0] :: int[12];
  const this.field_prime := split(field_prime) : [12][32];

  let vx = join(valueOf(args[0]));
  let r  = ref_incFieldPrime(vx);

  ensures valueOf(args[0]) := split(r.rslt) : [12][32];
  returns: r.carry;
  verifyUsing: abc;
};
