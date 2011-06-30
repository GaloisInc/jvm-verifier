/*
Module           : Div.java
Description      :
Stability        : provisional
Point-of-contact : atomb
*/

import com.galois.symbolic.Symbolic;

class Div {
  // Static large word operations {{{2

  static final int INT_MASK = 0xFFFF;

  /** Let w = z -p, and return carry bit. */
  static private boolean is_zero(int[] x) {
    for (int i = 0; i != x.length; ++i) {
      if (x[i] != 0) return false;
    }
    return true;
  }

  /**
   * Returns true if x equals y.
   */
  static private boolean is_equal(int[] x, int[] y) {
    for (int i = 0; i != x.length; ++i) {
      if (x[i] != y[i]) return false;
    }
    return true;
  }

  /**
   * Assigns x = y
   */
  static private void assign(int[] x, int[] y) {
    for (int i = 0; i != x.length; ++i) x[i] = y[i];
  }

  /**
   * Returns true if x is less than y.
  */
  static boolean lt(int[] x, int[] y) {
    int i = x.length;
    do {
      --i;
      if (x[i] != y[i]) {
        boolean xPos = 0 <= x[i];
        boolean yPos = 0 <= y[i];
        if (!yPos && xPos) return true;
        if (!xPos && yPos) return false;
        return x[i] < y[i];
      }
    } while (i != 0);
    return false;
  }

  /**
   * Sets all entries in x to zero.
   */
  static void set_zero(int[] x) {
    for (int i = 0; i != x.length; ++i) x[i] = 0;
  }

  /**
   * Sets all entries in x to zero.
   */
  static private void set_unit(int[] x) {
    x[0] = 1;
    for (int i = 1; i != x.length; ++i) x[i] = 0;
  }

  /**
   * Assigns <code>z = c ** (n-1) + (x &gt;&gt; 1)</code>
   */
  static private void shr(int[] z, int c, int[] x) {
    for (int i = z.length - 1; i != -1; --i) {
      int xi = x[i];
      z[i] = (c << 31) | (xi >>> 1);
      c = xi & 1;
    }
  }

  static int sub(int[] z, int[] x, int[] y) {
    int c = 0;
    for (int i = 0; i < z.length; ++i) {
      int vl = c + (x[i] & INT_MASK) - (y[i] & INT_MASK);
      int vh = (vl >> 16) + (x[i] >>> 16) - (y[i] >>> 16);
      z[i] = vl & INT_MASK | vh << 16;
      c = vh >> 16;
    }
    return c;
  }

  static int add(int[] z, int[] x, int[] y) {
    int w = z.length;
    int c = 0;
    for (int i = 0; i != w; ++i) {
      c += (x[i] & INT_MASK) + (y[i] & INT_MASK);
      int v = (c >> 16) + (x[i] >>> 16) + (y[i] >>> 16);
      z[i] = c & INT_MASK | v << 16;
      c = v >> 16;
    }
    return c;
  }


  // Generic field operations {{{2

  /**
   * Assigns z = x - y (mod p).
   */
  static void mod_sub(int[] z, int[] x, int[] y, int[] p) {
    if (sub(z, x, y) != 0) add(z, z, p);
  }

  /** Assigns x = x / 2 (mod p). */
  static void mod_half(int[] x, int[] p) {
    // If z[0] is odd
    if ((x[0] & 0x1) != 0) {
      shr(x, add(x, x, p), x);
    } else {
      shr(x, 0, x);
    }
  }

  /**
   * Assigns ra = x / y mod p.
   * Uses buffers a, b, ra, and rb to avoid allocating memory.
   * x,y, and p are unchanged.
   * Buffers must be distinct from each other as well as x,y, and p.
   */
    static void mod_div(int[] ra, int[] x, int[] y, int[] p, int[] a, int[] b, int[] rb) {
    assign(a, p);
    set_zero(ra);
    assign(rb, x);
    assign(b, y);

    boolean swapped = false;
    while (!is_zero(b)) {
      // If odd
      if ((b[0] & 1) != 0) {
        if (!lt(a, b)) {
          int[] t = a;
          a = b;
          b = t;

          t = ra;
          ra = rb;
          rb = t;
          swapped = !swapped;
        }

        // b <- b - a
        sub(b, b, a);
        // rb <- rb - ra
        mod_sub(rb, rb, ra, p);
      }
      // b <- b >> 1
      shr(b, 0, b);
      // rb <- half(rb)
      mod_half(rb, p);
    }

    if(swapped) assign(rb, ra);
  }

  static boolean div_step(int[] a, int[] ra, int[] b, int[] rb, int[] p, boolean swapped) {

      // If odd
      if ((b[0] & 1) != 0) {
        if (!lt(a, b)) {
          int[] t = a;
          a = b;
          b = t;

          t = ra;
          ra = rb;
          rb = t;
          swapped = !swapped;
        }

        // b <- b - a
        sub(b, b, a);
        // rb <- rb - ra
        mod_sub(rb, rb, ra, p);
      }
      // b <- b >> 1
      shr(b, 0, b);
      // rb <- half(rb)
      mod_half(rb, p);

      return swapped;
  }

  static void gen_set_zero() {
      int[] x = Symbolic.freshIntArray(12);
      set_zero(x);
      Symbolic.writeAiger("java_set_zero.aig", x);
  }

  static void gen_set_unit() {
      int[] x = Symbolic.freshIntArray(12);
      set_unit(x);
      Symbolic.writeAiger("java_set_unit.aig", x);
  }

  static void gen_assign() {
      int[] a = Symbolic.freshIntArray(12);
      int[] r = new int[12];
      assign(r, a);
      Symbolic.writeAiger("java_assign.aig", r);
  }

  static void gen_mod_half() {
      int[] x = Symbolic.freshIntArray(12);
      int[] p = Symbolic.freshIntArray(12);
      mod_half(x, p);
      Symbolic.writeAiger("java_mod_half.aig", x);
  }

  static void gen_shr() {
      int c = Symbolic.freshInt(0);
      int[] x = Symbolic.freshIntArray(12);
      shr(x, c & 1, x);
      Symbolic.writeAiger("java_shr.aig", x);
  }

  static void gen_sub() {
      int[] x = Symbolic.freshIntArray(12);
      int[] y = Symbolic.freshIntArray(12);
      int[] r = new int[12];
      sub(r, x, y);
      Symbolic.writeAiger("java_sub.aig", r);
  }

  static void gen_mod_sub() {
      int[] x = Symbolic.freshIntArray(12);
      int[] y = Symbolic.freshIntArray(12);
      int[] p = Symbolic.freshIntArray(12);
      int[] r = new int[12];
      mod_sub(r, x, y, p);
      Symbolic.writeAiger("java_mod_sub.aig", r);
  }

  static void gen_add() {
      int[] x = Symbolic.freshIntArray(12);
      int[] y = Symbolic.freshIntArray(12);
      int[] r = new int[12];
      add(r, x, y);
      Symbolic.writeAiger("java_add.aig", r);
  }

  static void gen_lt() {
      int[] x = Symbolic.freshIntArray(12);
      int[] y = Symbolic.freshIntArray(12);
      boolean r;
      r = lt(x, y);
      Symbolic.writeAiger("java_lt.aig", r);
  }

  static int boolToInt(boolean b) {
      if(b) { return 1; } else { return 0; }
  }

  static void gen_div_step() {
    int[] a = Symbolic.freshIntArray(12);
    int[] ra = Symbolic.freshIntArray(12);
    int[] b = Symbolic.freshIntArray(12);
    int[] rb = Symbolic.freshIntArray(12);
    int[] p = Symbolic.freshIntArray(12);
    boolean swapped = Symbolic.freshBoolean(false);
    int[] result = new int[4 * 12 + 1];
    int i;
    boolean s;
    s = div_step(a, ra, b, rb, p, swapped);
    for(i = 0; i < 12; i++) {
        result[i +  0] =  a[i];
        result[i + 12] = ra[i];
        result[i + 24] =  b[i];
        result[i + 36] = rb[i];
    }
    result[4*12] = boolToInt(s);
    Symbolic.writeAiger("java_div_step.aig", result);
  }

  public static void main(String[] args) {
    //gen_assign();
    //gen_mod_half();
    //gen_shr();
    //gen_set_unit();
    //gen_sub();
    //gen_lt();
    //gen_add();
    //gen_mod_sub();
    gen_div_step();
  }
}
