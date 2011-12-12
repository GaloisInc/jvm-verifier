method com.galois.ecc.P384ECC64.group_mul
{
  var r, x, y          :: int[12];
  var this.group_order :: int[12];
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  let jx = join(valueOf(x));
  let jy = join(valueOf(y));
  let jg = join(valueOf(this.group_order)); 
  ensure valueOf(r) := split(ref_mod_mul(jg, jx, jy)) : [12][32];
  //quickcheck 1;
  //verify{ rewrite; yices; };
};
