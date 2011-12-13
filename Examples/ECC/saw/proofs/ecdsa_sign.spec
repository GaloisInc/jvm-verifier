method com.galois.ecc.P384ECC64.signHash
{
  var args[0]                         :: com.galois.ecc.Signature;
  var args[1]                         :: int[12];
  var args[2]                         :: int[12];
  var args[3]                         :: int[12];
  var this.a                          :: int[24];
  var this.h                          :: int[12];
  var this.t1, this.t2, this.t3       :: int[12];
  var this.u1, this.u2                :: int[12];
  var args[0].r, args[0].s            :: int[12];

  var this.width                      :: int;
  var this.field_prime                :: int[12];
  var this.field_unit                 :: int[12];
  var this.group_order                :: int[12];
  var this.rP                         :: com.galois.ecc.JacobianPoint;
  var this.rP.x                       :: int[12];
  var this.rP.y                       :: int[12];
  var this.rP.z                       :: int[12];
  var this.sPtP                       :: com.galois.ecc.JacobianPoint;
  var this.sPtP.x                     :: int[12];
  var this.sPtP.y                     :: int[12];
  var this.sPtP.z                     :: int[12];
  var this.sMtP                       :: com.galois.ecc.JacobianPoint;
  var this.sMtP.x                     :: int[12];
  var this.sMtP.y                     :: int[12];
  var this.sMtP.z                     :: int[12];
  var this.sPt                        :: com.galois.ecc.AffinePoint;
  var this.sPt.x                      :: int[12];
  var this.sPt.y                      :: int[12];
  var this.sMt                        :: com.galois.ecc.AffinePoint;
  var this.sMt.x                      :: int[12];
  var this.sMt.y                      :: int[12];
  var this.basePoint                  :: com.galois.ecc.AffinePoint;
  var this.basePoint.x                :: int[12];
  var this.basePoint.y                :: int[12];
  var this.basePoint3                 :: com.galois.ecc.AffinePoint;
  var this.basePoint3.x               :: int[12];
  var this.basePoint3.y               :: int[12];
  var this.basePoint5                 :: com.galois.ecc.AffinePoint;
  var this.basePoint5.x               :: int[12];
  var this.basePoint5.y               :: int[12];

  let b3 = {
    x = 0x077a41d4606ffa1464793c7e5fdc7d98cb9d3910202dcd06bea4f240d3566da6b408bbae5026580d02d7e5c70500c831 : [384];
    y = 0xc995f7ca0b0c42837d0bbe9602a9fc998520b41c85115aa5f7684c0edc111eacc24abd6be4b5d298b65f28600a2f1df1 : [384]
  };

  let b5 = {
    x = 0x11de24a2c251c777573cac5ea025e467f208e51dbff98fc54f6661cbe56583b037882f4a1ca297e60abcdbc3836d84bc : [384];
    y = 0x8fa696c77440f92d0f5837e90a00e7c5284b447754d5dee88c986533b6901aeb3177686d0ae8fb33184414abe6c1713a : [384]
  };

  assert valueOf(this.basePoint.x)  := split(basePoint.x) : [12][32];
  assert valueOf(this.basePoint.y)  := split(basePoint.y) : [12][32];
  assert valueOf(this.basePoint3.x) := split(b3.x)        : [12][32];
  assert valueOf(this.basePoint3.y) := split(b3.y)        : [12][32];
  assert valueOf(this.basePoint5.x) := split(b5.x)        : [12][32];
  assert valueOf(this.basePoint5.y) := split(b5.y)        : [12][32];
  assert valueOf(this.group_order)  := split(group_order) : [12][32];
  assert valueOf(this.field_prime)  := split(field_prime) : [12][32];
  assert valueOf(this.field_unit)   := split(1:[384])     : [12][32];
  assert this.width                 := 12                 : [32];

  let d = join(valueOf(args[1]));
  let e = join(valueOf(args[2]));
  let k = join(valueOf(args[3]));
  assert not(d == 0:[384]);
  assert not(k == 0:[384]);
  assert not(group_order <=u d);
  assert not(group_order <=u k);

  let res = ref_ecdsa_sign(d, e, k);

  ensure valueOf(args[0].r) := split(res.r) : [12][32];
  ensure valueOf(args[0].s) := split(res.s) : [12][32];
  return res.r != 0:[384] && res.s != 0:[384];

  modify valueOf(this.a), valueOf(this.h);
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  modify valueOf(this.u1), valueOf(this.u2);
  modify valueOf(this.rP.x), valueOf(this.rP.y), valueOf(this.rP.z);
  modify valueOf(this.sPtP.x), valueOf(this.sPtP.y), valueOf(this.sPtP.z);
  modify valueOf(this.sMtP.x), valueOf(this.sMtP.y), valueOf(this.sMtP.z);
  modify valueOf(this.sPt.x), valueOf(this.sPt.y);
  modify valueOf(this.sMt.x), valueOf(this.sMt.y);
  modify valueOf(this.basePoint5.y);
  modify valueOf(this.basePoint5.x);
  modify valueOf(this.basePoint3.y);
  modify valueOf(this.basePoint3.x);
  modify valueOf(this.basePoint.y);
  modify valueOf(this.basePoint.x);
  modify valueOf(this.group_order);
  modify valueOf(this.field_unit);
  modify valueOf(this.field_prime);
  modify valueOf(args[3]);
  modify valueOf(args[2]);
  modify valueOf(args[1]);
  verify { rewrite; yices; };
};
