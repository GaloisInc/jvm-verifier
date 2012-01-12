method com.galois.ecc.P384ECC64.sq_loop
{
  var a       :: int[24];
  var x       :: int[12];

  let ja  = join(valueOf(a)) : [768];
  let jx  = join(valueOf(x)) : [384];
  let res = ref_sq_java_loop(ja, jx);

  ensure valueOf(a) := split(res) : [24][32];

  verify rewrite;
};
