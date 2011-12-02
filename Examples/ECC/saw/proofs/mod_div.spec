method com.galois.ecc.P384ECC64.mod_div {
  var args[0], args[1], args[2], args[3] :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];


  ensure valueOf(args[0]) 
    := split(p384_prime_div(join(valueOf(args[3])),
                            join(valueOf(args[1])),
                            join(valueOf(args[2])))) : [12][32];
  modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);

  at 44 {
    var swapped :: boolean;
    var a, ra, b, rb :: int[12];
    var this.t1, this.t2, this.t3 :: int[12];

    if (swapped == 0:[32]) {
      assert swapped := false;
      assert this.t1 := a;
      assert this.t2 := b;
      assert this.t3 := rb;

      ensure valueOf(ra) 
       := split(p384_egcd(join(valueOf(a)),
                          join(valueOf(ra)),
                          join(valueOf(b)),
                          join(valueOf(rb)))) : [12][32];
    } else {
      assert swapped := true;
      assert this.t1 := b;
      assert this.t2 := a;
      assert this.t3 := ra;

      ensure valueOf(rb) 
       := split(p384_egcd(join(valueOf(a)),
                          join(valueOf(ra)),
                          join(valueOf(b)),
                          join(valueOf(rb)))) : [12][32];
    };

    modify valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  };

  /*
  verify {
    if (initialPC == 44) {
       expand p384_egcd(join(valueOf(a)),
                        join(valueOf(ra)),
                        join(valueOf(b)),
                        join(valueOf(rb)));
    }
    uninterpret p384_egcd;
    yices;
  };
  */
};
