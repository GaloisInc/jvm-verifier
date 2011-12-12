method com.galois.ecc.P384ECC64.group_add
{
  var z, x, y          :: int[12];
  var this.group_order :: int[12];
  mayAlias { z, x, y };
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  let jx = join(valueOf(x));
  let jy = join(valueOf(y));
  ensure valueOf(z) := split(ref_group_add(jx, jy)) : [12][32];
  verify{ rewrite; yices; };
};
