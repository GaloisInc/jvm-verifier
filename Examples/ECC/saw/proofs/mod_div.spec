method com.galois.ecc.P384ECC64.mod_div {
  var ra, x, y, p :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];

  ensure valueOf(ra) 
    := split(ref_mod_div(join(valueOf(p)),
                         join(valueOf(x)),
                         join(valueOf(y)))) : [12][32];
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);

  from line +7 {
    var swapped :: boolean;
    var a, ra, b, rb, p :: int[12];
    var this.t1, this.t2, this.t3 :: int[12];

    let res
      = split(ref_egcd(join(valueOf(p)),
                        join(valueOf(a)),
                        join(valueOf(ra)),
                        join(valueOf(b)),
                        join(valueOf(rb)))) : [12][32];

    if (swapped == 0:[32]) {
      assert swapped := false;
      assert this.t1 := a;
      assert this.t2 := b;
      assert this.t3 := rb;

      ensure valueOf(ra) := res;
    } else {
      assert swapped := true;
      assert this.t1 := b;
      assert this.t2 := a;
      assert this.t3 := ra;

      ensure valueOf(rb) := res;
    }

    modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  }

  //quickcheck 20;
  verify {
    from line +7 {
       expand ref_egcd(join(valueOf(p)),
                       join(valueOf(a)),
                       join(valueOf(ra)),
                       join(valueOf(b)),
                       join(valueOf(rb)));
    }
    yices;
  }
};
