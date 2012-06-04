method com.galois.ecc.P384ECC64.sq
{
  var a :: int[24];
  var x :: int[12];
  let a' = join(valueOf(a)) : [768];
  let x' = join(valueOf(x)) : [384];
  ensure valueOf(a) := split(ref_sq_java(a', x')) : [24][32];
  verify rewrite;
};
