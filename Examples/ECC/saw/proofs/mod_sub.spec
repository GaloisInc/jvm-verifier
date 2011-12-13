method com.galois.ecc.P384ECC64.mod_sub 
{
  var z, x, y, p :: int[12];
  mayAlias { z, x, y };

  let jx = join(valueOf(x));
  let jy = join(valueOf(y));
  let jp = join(valueOf(p));
  ensure valueOf(z) := split(ref_mod_sub(jp, jx, jy)) : [12][32];

  verify abc;
};
