method com.galois.ecc.P384ECC64.incFieldPrime 
{
  var x :: int[12];
  let r = ref_incFieldPrime(join(valueOf(x)));
  ensure valueOf(x) := split(r.rslt) : [12][32];
  return r.carry;
  verify { rewrite; yices; };
};
