// TODO: PLACEHOLDER, currently does nothing useful
method com.galois.ecc.P384ECC64.mod_div 
{
  var args[0], args[1], args[2], args[3] :: int[12];
  var this.t1, this.t2, this.t3 :: int[12];
  modifies: valueOf(this.t1), valueOf(this.t2), valueOf(this.t3);
  verifyUsing: skip;
};

