method com.galois.ecc.P384ECC64.mul_joel
{
  var a   :: int[24];
  var x,y :: int[12];
  mayAlias { x, y };
  let a' = join(valueOf(a)) : [768];
  let x' = join(valueOf(x)) : [384];
  let y' = join(valueOf(y)) : [384];
  ensure valueOf(a) := split(ref_mul_joel(a', x', y')) : [24][32];
  //quickcheck 10;
  modify valueOf(y);
  modify valueOf(x);
  verify { rewrite; /*yices;*/ };
  //verify {abc;} ;
};
