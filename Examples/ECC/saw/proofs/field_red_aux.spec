method com.galois.ecc.P384ECC64.field_red_aux
{
  var z :: int[12];
  var a :: int[24];
  let r = ref_field_mod_aux(join(valueOf(a)));
  ensure valueOf(z) := split(r.rslt) : [12][32];
  return r.carry;
  verify { abc; };
};
