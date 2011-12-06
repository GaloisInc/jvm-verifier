import "rules.saw";
enable ref_ec_mul_aux;
method com.galois.ecc.P384ECC64.ec_mul_aux
{
  var r                         :: com.galois.ecc.JacobianPoint;
  var s                         :: com.galois.ecc.AffinePoint;
  var j, hi                     :: int;
  var i_lt_11                   :: boolean;
  var d_at_i, d_at_ip1          :: int;
  var this.a                    :: int[24];
  var this.t1, this.t2, this.t3 :: int[12];
  var r.x, r.y, r.z             :: int[12];
  var s.x, s.y                  :: int[12];
  var this.field_prime          :: int[12];

  assert j >=u 0:[32] && j <u 384:[32];                      // 0 <= j < 384
  assert i_lt_11 >=u 0:[32] && i_lt_11 <=u 1:[32];           // i_lt_11 is a boolean
  assert ((j >>u 5:[32]) <u 11:[32]) == (i_lt_11 >u 0:[32]); // i < 11 <=> i_lt_11
  assert valueOf(this.field_prime) := split(field_prime) : [12][32];

  let res = ref_ec_mul_aux( { x = join(valueOf(r.x))
                            ; y = join(valueOf(r.y))
                            ; z = join(valueOf(r.z))
                            }
                          , { x = join(valueOf(s.x))
                            ; y = join(valueOf(s.y))
                            }
                          , j, hi, i_lt_11, d_at_i, d_at_ip1 );
  ensure valueOf(r.x) := split(res.x) : [12][32];
  ensure valueOf(r.y) := split(res.y) : [12][32];
  ensure valueOf(r.z) := split(res.z) : [12][32];
  modify valueOf(this.a), valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);

//  quickcheck 10;
  verify { rewrite; yices; };
// NB: The version of ec_mul_merge_aux at 94c87f64 discharges using the following.
// verify { disable imp_true_elim1; rewrite; enable imp_true_elim1; rewrite; };
};
