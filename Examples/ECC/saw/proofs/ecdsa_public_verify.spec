method com.galois.ecc.P384ECC64.verifySignature
{
  var args[0]                         :: int[12];
  var args[1]                         :: com.galois.ecc.Signature;
  var args[2]                         :: com.galois.ecc.PublicKey;
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[1].r, args[1].s            :: int[12];
  var args[2].x, args[2].y            :: int[12];

  var this.field_prime :: int[12];
  var this.group_order :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let hash = join(valueOf(args[0]));
  let r = join(valueOf(args[1].r));
  let s = join(valueOf(args[1].s));
  let qx = join(valueOf(args[2].x));
  let qy = join(valueOf(args[2].y));
  assert r != 0:[384];
  assert s != 0:[384];
  assert hash <u g;
  assert r <u g;
  assert s <u g;

  let res = ref_ecdsa_public_verify(e, r, s, {x = qx; y = qy});

  return res;

  modify valueOf(this.a), valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  //quickcheck 1;
  verify { rewrite; /*yices; smtlib;*/ };
};
