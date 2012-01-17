method com.galois.ecc.P384ECC64.group_red
{
  var r                :: int[12];
  var c                :: long;
  var this.group_order :: int[12];
  assert valueOf(this.group_order) := split(group_order) : [12][32];
  let jr = join(valueOf(r));
  ensure valueOf(r) := split(ref_group_red(group_order, jr, c)) : [12][32];
  verify { rewrite; yices; };
  //quickcheck 10;
};
