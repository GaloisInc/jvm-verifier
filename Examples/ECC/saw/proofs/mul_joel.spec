method com.galois.ecc.P384ECC64.mul_joel
{
  var a   :: int[24];
  var x,y :: int[12];
  mayAlias { x, y };
  let x' = join(valueOf(x)) : [384];
  let y' = join(valueOf(y)) : [384];
  ensure valueOf(a) := split(ref_mul_joel(x', y')) : [24][32];
//  quickcheck 100;
//  modify valueOf(y);
//  modify valueOf(x);
//  verify { rewrite; yices; };
  verify {abc;} ;
};
