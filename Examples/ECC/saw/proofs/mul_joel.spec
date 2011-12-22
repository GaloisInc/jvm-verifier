method com.galois.ecc.P384ECC64.mul_joel
{
  var a   :: int[20];
  var x,y :: int[10];
  mayAlias { x, y };
  let x' = join(valueOf(x)) : [320];
  let y' = join(valueOf(y)) : [320];
  ensure valueOf(a) := split(ref_mul_joel(x', y')) : [20][32];
//  quickcheck 100;
//  modify valueOf(y);
//  modify valueOf(x);
  verify { rewrite; yices; };
};
