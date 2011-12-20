method com.galois.ecc.P384ECC64.signHash
{
  var signature                 :: com.galois.ecc.Signature;
  var privateKey                :: int[12];
  var hashValue                 :: int[12];
  var ephemeralKey              :: int[12];
  var this.a                    :: int[24];
  var this.h                    :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  var this.u1, this.u2          :: int[12];
  var signature.r, signature.s  :: int[12];
  var this.width                :: int;
  var this.field_prime          :: int[12];
  var this.field_unit           :: int[12];
  var this.group_order          :: int[12];
  var this.rP                   :: com.galois.ecc.JacobianPoint;
  var this.rP.x                 :: int[12];
  var this.rP.y                 :: int[12];
  var this.rP.z                 :: int[12];
  var this.sPtP                 :: com.galois.ecc.JacobianPoint;
  var this.sPtP.x               :: int[12];
  var this.sPtP.y               :: int[12];
  var this.sPtP.z               :: int[12];
  var this.sMtP                 :: com.galois.ecc.JacobianPoint;
  var this.sMtP.x               :: int[12];
  var this.sMtP.y               :: int[12];
  var this.sMtP.z               :: int[12];
  var this.sPt                  :: com.galois.ecc.AffinePoint;
  var this.sPt.x                :: int[12];
  var this.sPt.y                :: int[12];
  var this.sMt                  :: com.galois.ecc.AffinePoint;
  var this.sMt.x                :: int[12];
  var this.sMt.y                :: int[12];
  var this.basePoint            :: com.galois.ecc.AffinePoint;
  var this.basePoint.x          :: int[12];
  var this.basePoint.y          :: int[12];
  var this.basePoint3           :: com.galois.ecc.AffinePoint;
  var this.basePoint3.x         :: int[12];
  var this.basePoint3.y         :: int[12];
  var this.basePoint5           :: com.galois.ecc.AffinePoint;
  var this.basePoint5.x         :: int[12];
  var this.basePoint5.y         :: int[12];

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

  let d = join(valueOf(privateKey));
  let e = join(valueOf(hashValue));
  let k = join(valueOf(ephemeralKey));
  assert not(d == 0:[384]);
  assert not(k == 0:[384]);
  assert not(group_order <=u d);
  assert not(group_order <=u k);

  let res = ref_ecdsa_sign(d, e, k);

  ensure valueOf(signature.r) := split(res.r) : [12][32];
  ensure valueOf(signature.s) := split(res.s) : [12][32];
  return res.r != 0:[384] && res.s != 0:[384];

  ensure valueOf(this.h)      := split(0 : [384]) : [12][32];
  ensure valueOf(this.t1)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.t2)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.t3)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.u1)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.u2)     := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.x)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.y)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.rP.z)   := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.x) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.y) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPtP.z) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.x) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.y) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMtP.z) := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPt.x)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sPt.y)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMt.x)  := split(0 : [384]) : [12][32];
  ensure valueOf(this.sMt.y)  := split(0 : [384]) : [12][32];

  modify valueOf(this.a);
  modify valueOf(hashValue);
  verify { rewrite; yices; };
};
