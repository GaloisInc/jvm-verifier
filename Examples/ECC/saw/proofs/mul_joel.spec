method com.galois.ecc.P384ECC64.mul_joel
{
  var a   :: int[4]; // 24
  var x,y :: int[2]; // 12
  mayAlias { x, y };
  let x' = join(valueOf(x)) : [64];
  let y' = join(valueOf(y)) : [64];
  ensure valueOf(a) := split(ref_mul_joel(x', y')) : [4][32];
  //quickcheck 10;
  modify valueOf(y);
  modify valueOf(x);
  verify { rewrite; /*yices;*/ };
  //verify {abc;} ;
};
