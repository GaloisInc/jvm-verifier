// rule eq_elim: forAll {x:a}. x == x -> true;

// rule join_split_384: forAll {x:[384]}. join(split(x) : [12][32]) -> x;
// rule join_split_768: forAll {x:[768]}. join(split(x) : [24][32]) -> x;

method com.galois.ecc.P384ECC64.knarFmul
{
  var a :: int[6];
  var x, y :: int[3];

  mayAlias { x, y };

  ensure
//    valueOf(a) := javaish_mul_3_32(valueOf(x), valueOf(y));
    valueOf(a) := truncated_1_javaish_mul_2_32(valueOf(x), valueOf(y));

  verify { abc; };
};
