rule eq_elim: forAll {x:a}. x == x -> true;

rule join_split_384: forAll {x:[384]}. join(split(x) : [12][32]) -> x;
rule join_split_768: forAll {x:[768]}. join(split(x) : [24][32]) -> x;

method com.galois.ecc.P384ECC64.mul
{
  var args[0] :: int[4];
  var args[1], args[2] :: int[2];

  mayAlias { args[1], args[2] };

  let x = join(valueOf(args[1]));
  let y = join(valueOf(args[2]));

  ensure
    valueOf(args[0]) := split(nat_mul(x, y)) : [4][32];

  verify { abc; };
};
