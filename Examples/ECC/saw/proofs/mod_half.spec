method com.galois.ecc.P384ECC64.mod_half
{
  var x, p :: int[12]; 
  mayAlias { x, p };
  let jx = join(valueOf(x));
  let jp = join(valueOf(p));
  // NB: ref_mod_half takes it parameters in the opposite order of mod_half.
  ensure valueOf(x) := split(ref_mod_half(jp, jx)) : [12][32];
  verify abc;
};

