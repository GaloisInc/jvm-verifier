method com.galois.ecc.P384ECC64.group_red_aux
{
  var r              :: int[12];
  var aj, j          :: int;
  var c, b           :: long;
  assert (j >=s 0 :[32]) && (j <=s 11:[32]);
  let jr = join(valueOf(r));
  let res = ref_group_red_aux(jr, aj, j, c, b);
  ensure valueOf(r) := split(res.gra_r) : [12][32];
  return res.gra_b;
  verify yices;
};
