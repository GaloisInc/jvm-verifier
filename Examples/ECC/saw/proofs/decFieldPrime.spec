
method com.galois.ecc.P384ECC64.decFieldPrime
{
  var x                :: int[12];
  var this.field_prime :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];
  let r = ref_decFieldPrime(join(valueOf(x)));
  ensure valueOf(x) := split(r.rslt) : [12][32];
  return r.carry;
  verify { rewrite; yices; };
};
