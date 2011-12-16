method com.galois.ecc.P384ECC64.ec_twin_mul_aux1
{
  var r                          :: com.galois.ecc.JacobianPoint;
  var u0, u1                     :: int;
  var sPt, s, sMt, t             :: com.galois.ecc.AffinePoint;
  var r.x, r.y, r.z              :: int[12];
  var sPt.x, sPt.y               :: int[12];
  var s.x, s.y                   :: int[12];
  var sMt.x, sMt.y               :: int[12];
  var t.x, t.y                   :: int[12];
  var this.a                     :: int[24];
  var this.t1, this.t2, this.t3  :: int[12];
  var this.field_prime           :: int[12];

  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = 
    ref_ec_twin_mul_aux1( { x = join(valueOf(r.x)); y = join(valueOf(r.y)); z = join(valueOf(r.z)) }
                        , u0
                        , u1
                        , { x = join(valueOf(sPt.x)) ; y = join(valueOf(sPt.y)) }
                        , { x = join(valueOf(s.x))   ; y = join(valueOf(s.y))   }
                        , { x = join(valueOf(sMt.x)) ; y = join(valueOf(sMt.y)) }
                        , { x = join(valueOf(t.x))   ; y = join(valueOf(t.y))   }
                        );

  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];

  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  //quickcheck 15000;
  verify { rewrite; yices; /*smtlib;*/ };
};
