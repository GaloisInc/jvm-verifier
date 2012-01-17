method com.galois.ecc.P384ECC64.group_mul_aux
{
  var r              :: int[12];
  var yj, j, xi      :: int;
  var c              :: long;
  assert (j >=s 0 :[32]) && (j <=s 11:[32]);
  let jr = join(valueOf(r));
  let res = ref_group_mul_aux(jr, yj, j, xi, c);
  ensure valueOf(r) := split(res.gra_r) : [12][32];
  return res.gra_b;
  verify yices;
};
