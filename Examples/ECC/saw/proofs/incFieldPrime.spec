
method com.galois.ecc.P384ECC64.incFieldPrime 
{
  var args[0] :: int[12];

  let vx = join(valueOf(args[0]));
  let r  = ref_incFieldPrime(vx);

  ensure valueOf(args[0]) := split(r.rslt) : [12][32];
  return r.carry;
  verify yices;
};
