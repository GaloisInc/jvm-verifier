method com.galois.ecc.P384ECC64.signHash
{
  var args[0]                         :: com.galois.ecc.Signature;
  var args[1]                         :: int[12];
  var args[2]                         :: int[12];
  var args[3]                         :: int[12];
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var args[0].r, args[0].s            :: int[12];

  var this.field_prime :: int[12];
  var this.group_order :: int[12];
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let d = join(valueOf(args[1]));
  let e = join(valueOf(args[2]));
  let k = join(valueOf(args[3]));
  let g = join(valueOf(this.group_order));
  assert d != 0:[384];
  assert k != 0:[384];
  assert d <u g;
  assert e <u g;
  assert k <u g;

  let res = ref_ecdsa_sign(d, e, k);

  ensure valueOf(args[0].r) := split(res.r) : [12][32];
  ensure valueOf(args[0].s) := split(res.s) : [12][32];
  return res.r == 0:[384] || res.s == 0:[384];

  modify valueOf(this.a), valueOf(this.h), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  //quickcheck 1;
  verify { rewrite; /*yices; smtlib;*/ };
};
