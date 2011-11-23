/*
Module           : ECCProvider.java
Description      :
Stability        : provisional
Point-of-contact : jhendrix
*/

package com.galois.ecc;

// Class definition {{{1

/**
 * Abstract class that provides Elliptic Curve cryptography services
 * for a particular curve.
 */
public abstract class ECCProvider {
  // Constants {{{2

  /** 
   * Number of elements required in array for storing field and group values.
   */
  private final int width;

  /** 
   * Prime used in field computations.
   */
  protected final int[] field_prime;

  /**
   * Unit value used in field.
   */
  private final int[] field_unit;

  /**
   * Base point for curve subgroup.
   */
  private final AffinePoint basePoint;

  /**
   * 3 * base point for curve subgroup.
   */
  private final AffinePoint basePoint3;

  /**
   * 5 * base point for curve subgroup.
   */
  private final AffinePoint basePoint5;

  /**
   * Order of subgroup of curve generated by base point.
   */
  protected final int[] group_order;

  // Intermediate values {{{2
  private int[] h;
  private int[] t1;
  private int[] t2;
  private int[] t3;
  private int[] u1;
  private int[] u2;
  private JacobianPoint rP;
  private JacobianPoint sPtP;
  private JacobianPoint sMtP;
  private AffinePoint sPt;
  private AffinePoint sMt;

  /**
   * Intermediate point for storing key.
   */
  private final AffinePoint qPoint;

  // Constructor and cleanup operations {{{2

  /**
   * Construct a new ECCProvider given the constants for configuration.
   */
  protected ECCProvider(int width, int[] field_prime, int[] field_unit, int[] group_order, AffinePoint basePoint) {
    this.width = width;
    this.group_order = group_order;
    this.field_prime = field_prime;
    this.field_unit = field_unit;
    this.basePoint = basePoint;

    h = new int[width];
    t1 = new int[width];
    t2 = new int[width];
    t3 = new int[width];
    u1 = new int[width];
    u2 = new int[width];

    rP = new JacobianPoint(width);
    sPtP = new JacobianPoint(width);
    sMtP = new JacobianPoint(width);
    sPt = new AffinePoint(width);
    sMt = new AffinePoint(width);
    qPoint = new AffinePoint(width);

    basePoint3 = new AffinePoint(width);
    basePoint5 = new AffinePoint(width);
  }

  /**
   * Performs precomputations necessary to compute multiplies of
   * base point.  Should be called in final constructor after
   * subclass constructors are called.
   */
  protected void init() {
    JacobianPoint s4 = new JacobianPoint(width);
    ec_projectify(s4, basePoint);
    ec_double(s4);
    ec_double(s4);

    JacobianPoint s3J = new JacobianPoint(width);
    copy_point(s3J, s4);
    ec_full_sub(s3J, basePoint);

    JacobianPoint s5J = new JacobianPoint(width);
    copy_point(s5J, s4);
    ec_full_add(s5J, basePoint);

    ec_affinify(basePoint3, s3J);
    ec_affinify(basePoint5, s5J);
  }

  /**
   * Clear all buffers used for storing intermediate values.
   */
  protected void cleanup() {
    set_zero(h);
    set_zero(t1);
    set_zero(t2);
    set_zero(t3);
    set_zero(u1);
    set_zero(u2);
    rP.clear();
    sPtP.clear();
    sMtP.clear();
    sPt.clear();
    sMt.clear();
  }

  // Static large word operations {{{2

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
   * Returns true if x is less than or equal to y.
  */
  static boolean leq(int[] x, int[] y) {
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
    return true;
  }

  /**
   * Sets all entries in x to zero.
   */
  static void set_zero(int[] x) {
    for (int i = 0; i != x.length; ++i) x[i] = 0;
  }

  static void jpf_delete_me(JacobianPoint r, JacobianPoint s) {
    assign(r.x, s.x);
    assign(r.y, s.y);
    assign(r.z, s.z);
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

  // Abstract operations {{{2
  // Large word operations {{{3
  /**
   * Assigns z = x + y, and returns carry bit.
   */
  protected abstract int add(int[] z, int[] x, int[] y);

  /**
   * Assigns z = x + x, and returns carry bit (0 or 1).
   */
  protected abstract int dbl(int[] z, int[] x);

  /**
   * Assigns z = z - 2 * y, and returns carry bit (-2, -1, or 0).
   */
  protected abstract int dbl_dec(int[] z, int[] y);

  /**
   * Assigns z = x - y, and returns carry bit (-1 or 0)
   */
  protected abstract int sub(int[] z, int[] x, int[] y);

  // Field operations {{{3

  /**
   * Assigns x = x - field_prime, and returns carry value (0 or -1).
   */
  protected abstract int decFieldPrime(int[] x);

  /**
   * Assigns x = x + field_prime, and returns carry value (0 or 1).
   */
  protected abstract int incFieldPrime(int[] x);

  /**
   * Assigns z = x * y (mod field_prime).
   */
  protected abstract void field_mul(int[] z, int[] x, int[] y);

  /**
   * Assigns z = x * x (mod field_prime).
   */
  protected abstract void field_sq(int[] z, int[] x);

  void field_mul_test(int[] z, int[] x, int[] y) { field_mul(z,x,y); }
  void field_sqr_test(int[] z, int[] x) { field_sq(z,x); }
  
  /**
   * Assigns z = x * y (mod group_order).
   */
  protected abstract void group_mul(int[] r, int[] x, int[] y);

  // Predefined field operations {{{2
  /**
   * Assigns z = x + y (mod field_prime).
   */
  public void field_add(int[] z, int[] x, int[] y) {
    if (add(z, x, y) != 0 || leq(field_prime, z)) decFieldPrime(z);
  }

  /**
   * Assigns y = 2*x (mod field_prime).
   */
  private void field_dbl(int[] z, int[] x) {
    if (dbl(z, x) != 0 || leq(field_prime, z)) decFieldPrime(z);
  }

  /**
   * Assigns z = z - 2 * x (mod field_prime).
   */
  private void field_dbl_dec(int[] z, int[] x) {
    field_sub(z, z, x);
    field_sub(z, z, x);
    /*
    int b = dbl_dec(z, x);
    if (b != 0) b += incFieldPrime(z);
    if (b != 0) b += incFieldPrime(z);
    if (b != 0) incFieldPrime(z);
    */
  }

  /**
   * Assigns z = 3 * x (mod field_prime).
   */
  public void field_mul3(int[] z, int[] x) {
    field_dbl(z, x);
    field_add(z, x, z);
    /*
    int c;
    c = dbl(z, x);
    if (c != 0) decFieldPrime(z);
    c = add(z, x, z);
    if (c != 0) c += decFieldPrime(z);
    if (c != 0 || leq(field_prime, z)) decFieldPrime(z);
    */
  }

  /**
   * Assigns z = 4 * x (mod field_prime).
   */
  private void field_mul4(int[] z, int[] x) {
    field_dbl(z, x);
    field_dbl(z, z);
          /*
    if (dbl(z, x) != 0) decFieldPrime(z);
    int c = dbl(z, z);
    if (c != 0) c += decFieldPrime(z);
    if (c != 0 || leq(field_prime, z)) decFieldPrime(z);
    */
  }

  /**
   * Assigns z = 8 * x (mod field_prime).
   */
  private void field_mul8(int[] z, int[] x) {
    field_dbl(z, x);
    field_dbl(z, z);
    field_dbl(z, z);
    /*
    int c;
    if (dbl(z, x) != 0) decFieldPrime(z);
    c = dbl(z, z);
    if (c != 0) c += decFieldPrime(z);
    if (c != 0) decFieldPrime(z);
    c = dbl(z, z);
    if (c != 0) c += decFieldPrime(z);
    if (c != 0 || leq(field_prime, z)) decFieldPrime(z);
    */
  }

  /**
   * Normalizes z to ensure it is less than field_prime.
   */
  private void field_normalize(int[] z) {
    if (leq(field_prime, z)) decFieldPrime(z);
  }

  /**
   * Assigns z = x - y (mod field_prime).
   */
  public void field_sub(int[] z, int[] x, int[] y) {
    int c = sub(z, x, y);
    if (c != 0) incFieldPrime(z);
  }

  /**
   * Assigns z = x + y (mod group_order).
   */
  private void group_add(int[] z, int[] x, int[] y) {
    int c = add(z, x, y);
    if (c != 0 || leq(group_order, z)) sub(z, z, group_order);
  }

  // Generic field operations {{{2

  /**
   * Assigns z = x - y (mod p).
   */
  private void mod_sub(int[] z, int[] x, int[] y, int[] p) {
    if (sub(z, x, y) != 0) add(z, z, p);
  }

  /** Assigns x = x / 2 (mod p). */
  private void mod_half(int[] x, int[] p) {
    // If x[0] is odd
    if ((x[0] & 0x1) != 0) {
      shr(x, add(x, x, p), x);
    } else {
      shr(x, 0, x);
    }
  }

  /**
   * Assigns ra = x / y mod p.
   * x,y, and p are unchanged.
   * Buffers must be distinct from each other as well as x,y, and p.
   * Overwrites t1, t2, and t2 with random data.
   */
  private void mod_div(int[] ra, int[] x, int[] y, int[] p) {
    int[] a = t1; assign(a, p);
    set_zero(ra);

    int[] b = t2;  assign(b, y);
    int[] rb = t3; assign(rb, x);

    boolean swapped = false;
    while (!is_zero(b)) {
      // If odd
      if ((b[0] & 1) != 0) {
        if (!leq(a, b)) {
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

    if (swapped) assign(rb, ra);
  }

  // Point operations {{{2

  /**
   * Assigns r the point represented by s.
   * Overwrites t1, t2, and t2 with random data.
   *
   * @param r An affine point to store result in.
   * @param s A non-zero Jacobian point.
   */
  private void ec_affinify(AffinePoint r, JacobianPoint s) {
    mod_div(r.x, field_unit, s.z, field_prime); // r.x = 1 / s.z
    field_mul(r.y, s.y, r.x); // r.y = s.y / s.z
    field_sq(r.x, r.x); // r.x = 1 / s.z^2
    field_mul(r.y, r.y, r.x); // r.y = s.y / s.z^3
    field_mul(r.x, r.x, s.x); // r.x = s.x / s.z^2
  }

  /**
   * Assigns r the point represented by s.
   *
   * @param r A Jacobian point to store result in.
   * @param s An affine point.
   */
  private static void ec_projectify(JacobianPoint r, AffinePoint s) {
    assign(r.x, s.x);
    assign(r.y, s.y);
    set_unit(r.z);
  }

  /**
   * Doubles r.
   * Modifies t1 and t2.
   *
   * @param r Point to double.
   * @param t4 A n-element array to store intermediate results.
   * @param t5 A n-element array to store intermediate results.
   */
  private void ec_double(JacobianPoint r) {
    int[] t4 = t1;
    int[] t5 = t2;
    if (is_zero(r.z)) {
      set_unit(r.x);
      set_unit(r.y);
      set_zero(r.z);
      return;
    }
    field_sq(t4, r.z);        //  7: t4 <- (t3)^2
    field_sub(t5, r.x, t4);   //  8: t5 <- t1 - t4
    field_add(t4, r.x, t4);   //  9: t4 <- t1 + t4
    field_mul(t5, t4, t5);    // 10: t5 <- t4 * t5
    field_mul3(t4, t5);       // 11: t4 <- 3 * t5
    field_mul(r.z, r.z, r.y); // 12: t3 <- t3 * t2
    field_dbl(r.z, r.z);      // 13: t3 <- 2 * t3
    field_sq(r.y, r.y);       // 14: t2 <- t2^2
    field_mul(t5, r.x, r.y);  // 15: t5 <- t1 * t2
    field_mul4(t5, t5);       // 16: t5 <- 4 * t5
    field_sq(r.x, t4);        // 17: t1 <- (t4)^2
    field_dbl_dec(r.x, t5);   // 18: t1 <- t1 - 2 * t5
    field_sq(r.y, r.y);       // 19: t2 <- (t2)^2
    field_mul8(r.y, r.y);     // 20: t2 <- 8 * t2
    field_sub(t5, t5, r.x);   // 21: t5 <- t5 - t1
    field_mul(t5, t4, t5);    // 22: t5 <- t4 * t5
    field_sub(r.y, t5, r.y);  // 23: t2 <- t5 - t2
  }

  /**
   * Assigns r = r + t.
   * Modifies t1, t2, and t3.
   *
   * @param r Point to modify.
   * @param t Point to add.
   * @return True if s and t are not the same point.
   */
  private void ec_full_add(JacobianPoint r, AffinePoint t) {
    int[] t4 = t1;
    int[] t5 = t2;
    int[] t7 = t3;

    if (is_zero(r.z)) {
      assign(r.x, t.x);
      assign(r.y, t.y);
      set_unit(r.z);
      return;
    }

    field_sq(t7, r.z);      //  9: t7 <- (t3)^2
    field_mul(t4, t.x, t7); // 10: t4 <- t4 * t7
    field_mul(t7, r.z, t7); // 11: t7 <- t3 * t7
    field_mul(t5, t.y, t7); // 12: t5 <- t5 * t7
    field_sub(t4, r.x, t4); // 13: t4 <- t1 - t4; t4 <- r.x - t.x * r.z^2
    field_sub(t5, r.y, t5); // 14: t5 <- t2 - t5

    if (is_zero(t4)) {
      if (is_zero(t5)) {
        ec_double(r);
      } else {
        set_unit(r.x);
        set_unit(r.y);
        set_zero(r.z);
      }
    } else {
      field_dbl(r.x, r.x); 
      field_sub(r.x, r.x, t4);    // 22: t1 <- 2*t1 - t4; r.x <- r.x + t.x * r.z^2
      field_dbl(r.y, r.y);
      field_sub(r.y, r.y, t5);    // 23: t2 <- 2*t2 - t5; r.y <- r.y + t.y * r.z^3

      field_mul(r.z, r.z, t4);    // 27: t3 <- t3 * t4; r.z <- r.z * (r.x - t.x * r.z^2)
      field_sq(t7, t4);           // 28: t7 <- (t4)^2 ; t7 <- (r.x - t.x * r.z^2)^2
      field_mul(t4, t4, t7);      // 29: t4 <- t4 * t7; t4 <- (r.x - t.x * r.z^2)^3
      field_mul(t7, r.x, t7);     // 30: t7 <- t1 * t7; t7 <- (r.x + t.x * r.z^2) * (r.x - t.x * r.z^2)^2
      field_sq(r.x, t5);          // 31: t1 <- (t5)^2;  r.x <- (r.y - t.y * r.z^3)^2
      field_sub(r.x, r.x, t7);    // 32: t1 <- t1 - t7; r.x <- (r.y - t.y * r.z^3)^2 - (r.x + t.x * r.z^2) * (r.x - t.x * r.z^2)^2
      field_dbl_dec(t7, r.x);     // 33: t7 <- t7 - 2*t1
      field_mul(t5, t5, t7);      // 34: t5 <- t5 * t7
      field_mul(t4, r.y, t4);     // 35: t4 <- t2 * t4
      field_sub(r.y, t5, t4);     // 36: t2 <- t5 - t4
      mod_half(r.y, field_prime); // 37: t2 <- t2/2
    }
  }

  /**
   * Assigns r = r - t.
   * Overwrites t1, t2, and t3.
   *
   * @param r Point to subtract t from.
   * @param t Point to subtract.
   * @param a Array with at least 2n-elements for storing intermediate results.
   * @return <code>true</code> if s and t are not the same point.
   */
  private void ec_full_sub(JacobianPoint r, AffinePoint t) {
    if (!is_zero(t.y)) sub(t.y, field_prime, t.y);
    ec_full_add(r, t);
    if (!is_zero(t.y)) sub(t.y, field_prime, t.y);
  }

  private static void copy_point(JacobianPoint r, JacobianPoint s) {
    assign(r.x, s.x);
    assign(r.y, s.y);
    assign(r.z, s.z);
  }

  /**
   * Assigns r = d * s.  As a side effect, this function uses <code>h</code>, <code>t1</code>, <code>t2</code>
   * and <code>t3</code> as temporary buffers that are overwritten.
   *
   * @param r Point to store result in.
   * @param d Scalar multiplier.
   * @param s Point of multiplicand.
   * @param h Temporary buffer with at least n-elements.
   */
  private void ec_mul(JacobianPoint r, int[] d, AffinePoint s) {
    shr(h, 0, d);
    // If h <- d + (d >> 1) overflows
    if (add(h, d, h) != 0) {
      // Start with r = s.
      assign(r.x, s.x);
      assign(r.y, s.y);
      set_unit(r.z);
    } else {
      // Otherwise start with r = 0.
      set_unit(r.x);
      set_unit(r.y);
      set_zero(r.z);
    }
    
    for (int j = 32 * h.length - 1; j >= 0; --j) {
      int i = j >>> 5;
      int m = 1 << j;
      int hi = h[i];
      int ki = d[i] >>> 1;
      if (i < 11) ki |= (d[i+1] & 1) << 31;
      ec_mul_merge_aux(r, s, m, hi, ki);
    }
  }

  // Auxiliary function for ec_mul to force path state merging by the simulator
  private void ec_mul_merge_aux(JacobianPoint r, AffinePoint s,
                                int m, int hi, int ki) {
    ec_double(r);

    if ((hi & m) != 0 && (ki & m) == 0) {
      ec_full_add(r, s);
    } else if ((hi & m) == 0 && (ki & m) != 0) {
      ec_full_sub(r, s);
    }
  }

  /**
   * Assigns r = d * s using a 2bit lookahead window.
   * As a side effect, this function uses <code>h</code>, <code>t1</code>, <code>t2</code>
   * and <code>t3</code> as temporary buffers that are overwritten.
   *
   * @param r Point to store result in.
   * @param d Scalar multiplier.
   * @param s Point to multiply.
   * @param s3 3 * Point to multiply.
   * @param s5 5 * Point to multiply.
   */
  private void ec_mul_window(JacobianPoint r, int[] d, AffinePoint s, AffinePoint s3, AffinePoint s5) {
    shr(h, 0, d);
    // If h <- d + d >> 1 overflows
    if (add(h, d, h) != 0) {
      // Start with r = s.
      assign(r.x, s.x);
      assign(r.y, s.y);
      set_unit(r.z);
    } else {
      // Otherwise start with r = 0.
      set_unit(r.x);
      set_unit(r.y);
      set_zero(r.z);
    }

    int j = 32 * h.length - 1;
    while (j >= 2) {
      int i = j >>> 5;
      int hi = h[i];
      int ki = d[i] >>> 1;
      if (i != h.length - 1) ki |= (d[i+1] & 1) << 31;
      int m = 1 << j;

      ec_double(r);
      if ((hi & m) != 0 && (ki & m) == 0) {
        j -= 2;
        i = j >>> 5;
        hi = h[i];
        ki = d[i] >>> 1;
        if (i != h.length - 1) ki |= (d[i+1] & 1) << 31;
        m = 1 << j;
        --j;

        if ((hi & m) != 0 && (ki & m) == 0) {
          ec_double(r);
          ec_double(r);
          ec_full_add(r, s5);
        } else if ((hi & m) == 0 && (ki & m) != 0) {
          ec_double(r);
          ec_double(r);
          ec_full_add(r, s3);
        } else {
          ec_full_add(r, s);
          ec_double(r);
          ec_double(r);
        }
      } else if ((hi & m) == 0 && (ki & m) != 0) {
        j -= 2;
        i = j >>> 5;
        hi = h[i];
        ki = d[i] >>> 1;
        if (i != h.length - 1) ki |= (d[i+1] & 1) << 31;
        m = 1 << j;
        --j;

        if ((hi & m) != 0 && (ki & m) == 0) {
          ec_double(r);
          ec_double(r);
          ec_full_sub(r, s3);
        } else if ((hi & m) == 0 && (ki & m) != 0) {
          ec_double(r);
          ec_double(r);
          ec_full_sub(r, s5);
        } else {
          ec_full_sub(r, s);
          ec_double(r);
          ec_double(r);
        }

      } else {
        --j;
      }
    }

    if (j == 1) {
      int hi = h[0];
      int ki = d[0];

      ec_double(r);
      if ((hi & 2) != 0 && (ki & 3) == 0) {
        ec_full_add(r, s);
        ec_double(r);
      } else if ((hi & 2) == 0 && (ki & 3) != 0) {
        ec_full_sub(r, s);
        ec_double(r);
      } else {
        ec_double(r);
        if ((hi & 1) != 0 && (ki & 2) == 0) {
          ec_full_add(r, s);
        } else if ((hi & 1) == 0 && (ki & 2) != 0) {
          ec_full_sub(r, s);
        }
      }

    } else if (j == 0) {
      int hi = h[0];
      int ki = d[0];
      ec_double(r);
      if ((hi & 1) != 0 && (ki & 2) == 0) {
        ec_full_add(r, s);
      } else if ((hi & 1) == 0 && (ki & 2) != 0) {
        ec_full_sub(r, s);
      }
    }
  }

  /**
   * Helper function used by twin multiplication.
   */
  private static int f(int t) {
    if ((18 <= t) && (t < 22)) return 9;
    if ((14 <= t) && (t < 18)) return 10;
    if ((22 <= t) && (t < 24)) return 11;
    if ((4 <= t) && (t < 12))  return 14;
    return 12;
  }

  /**
   * Assigns r = d0 * s + d1 * t.  As a side effect, this function uses
   * <code>h</code>, <code>t1</code>, <code>t2</code>
   * and <code>t3</code> as temporary buffers that are overwritten.
   *
   * @param r Point to store result in.
   * @param d0 First scalar multiplier.
   * @param s First point to multiply.
   * @param d1 Second scalar multiplier.
   * @param t Second point to multiply.
   */
  private void ec_twin_mul(JacobianPoint r,
                           int[] d0, AffinePoint s,
                           int[] d1, AffinePoint t,
                           JacobianPoint sPtP,
                           JacobianPoint sMtP,
                           AffinePoint sPt,
                           AffinePoint sMt) {
    // Special case for chance s == t or s == -t
    if (is_equal(s.x, t.x)) {
      int[] t0 = sPtP.x;
      if (is_equal(s.y, t.y)) { // s == t
        group_add(t0, d0, d1);
        ec_mul(r, t0, s);
      } else { // s == -t
        mod_sub(t0, d0, d1, group_order);
        ec_mul(r, t0, s);
      }
      return;
    }

    ec_projectify(sPtP, s);
    ec_full_add(sPtP, t);
    ec_projectify(sMtP, s);
    ec_full_sub(sMtP, t);

    field_mul(sPt.y, sPtP.z, sMtP.z);
    // Let h = 1 / (sPtP.z * sMtP.z)
    mod_div(h, field_unit, sPt.y, field_prime);

    // Let sPt = s + t
    field_mul(sPt.x, sMtP.z, h);    // sPt.x = 1 / sPtP.z
    field_mul(sPt.y, sPtP.y, sPt.x); // sPt.y = sPtP.y / sPtP.z
    field_sq(sPt.x, sPt.x);          // sPt.x = 1 / sPtP.z^2
    field_mul(sPt.y, sPt.y, sPt.x);  // sPt.y = sPtP.y / sPtP.z^3
    field_mul(sPt.x, sPtP.x, sPt.x); // sPt.x = sPtP.x / sPtP.z^2

    // Let sMt = s - t
    field_mul(sMt.x, sPtP.z, h);    // sMt.x = 1 / sMtP.z
    field_mul(sMt.y, sMtP.y, sMt.x); // sMt.y = sMtP.y / sMtP.z
    field_sq(sMt.x, sMt.x);          // sMt.x = 1 / sMtP.z^2
    field_mul(sMt.y, sMt.y, sMt.x);  // sMt.y = sMtP.y / sMtP.z^3
    field_mul(sMt.x, sMtP.x, sMt.x); // sMt.x = sMtP.x / sMtP.z^2

    int d0_11 = d0[11];
    int d1_11 = d1[11];

    int c0 = d0_11 >>> 28;
    int c1 = d1_11 >>> 28;

    int shift = 27;
    int e0 = d0_11;
    int e1 = d1_11;

    set_unit(r.x);
    set_unit(r.y);
    set_zero(r.z);

    for (int k = 379; k != -6; --k) {
      int h0 = c0 & 0x1F;
      if ((c0 & 0x20) != 0) h0 = 31 - h0;
      int h1 = c1 & 0x1F;
      if ((c1 & 0x20) != 0) h1 = 31 - h1;

      boolean h0Less = h0 < f(h1);
      boolean h1Less = h1 < f(h0);

      int u0 = h0Less ? 0 : ((c0 & 0x20) != 0 ? -1 : 1);
      int u1 = h1Less ? 0 : ((c1 & 0x20) != 0 ? -1 : 1);

      c0 = (h0Less ? 0 : 0x20) ^ (c0 << 1) | (e0 >>> shift) & 0x1;
      c1 = (h1Less ? 0 : 0x20) ^ (c1 << 1) | (e1 >>> shift) & 0x1;
  
      ec_double(r);
      if (u0 == -1) {
        if (u1 == -1) {
          ec_full_sub(r, sPt);
        } else if (u1 == 0) {
          ec_full_sub(r, s);
        } else {
          ec_full_sub(r, sMt);
        }
      } else if (u0 == 0) {
        if (u1 == -1) {
          ec_full_sub(r, t);
        } else if (u1 == 1) {
          ec_full_add(r, t);
        }
      } else { // u0 == 1
        if (u1 == -1) {
          ec_full_add(r, sMt);
        } else if (u1 == 0) {
          ec_full_add(r, s);
        } else {
          ec_full_add(r, sPt);
        }
      }

      if ((k & 0x1F) == 0) {
        // Get index of next element in d0 and d1.
        int i = (k >>> 5) - 1;
        shift = 31;
        if (i >= 0) {
          e0 = d0[i];
          e1 = d1[i];
        } else {
          e0 = 0;
          e1 = 0;
        }
      } else {
        --shift;
      }
    }
  }

  // ECDSA operations {{{2
  /**
   * Attempt to sign a given hash using a randomly generated ephermeral key.
   * The signing attempt may fail with probability <code>2/group_order</code>.
   * If so, <code>ecdsaSign</code> returns <code>false</code>, and should be
   * called with a fresh value of <code>k</code>.
   *
   * @param signature Signature to store result in.
   * @param privateKey Private key of signatory.
   *   that should be non-zero and less than group order.
   * @param hashValue Hash value generated by hash function, and converted as specified in appendix A.5.
   * @param ephemeralKey Random 12-element privateKey generated for this message signing
   *   that should be non-zero and less than group order.
   *
   * @return <code>true</code> if the signing attempt succeeds.
   * @throws NullPointerException Thrown if one of the parameters is <code>null</code>.
   * @throws IllegalArgumentException Thrown if a parameter does not have the corect size for this curve.
   */
  public boolean signHash(Signature signature, int[] privateKey, int[] hashValue, int[] ephemeralKey) {
    if (signature == null) throw new NullPointerException("signature");
    if (signature.r.length != width)
       throw new IllegalArgumentException("Unexpected signature size.");

    if (privateKey == null) throw new NullPointerException("privateKey");
    if (privateKey.length != width)
       throw new IllegalArgumentException("Unexpected private key size.");
    if (is_zero(privateKey))
      throw new IllegalArgumentException("privateKey is zero.");
    if (leq(group_order, privateKey))
      throw new IllegalArgumentException("privateKey must be less than group order.");

    if (hashValue == null) throw new NullPointerException("hashValue");
    if (hashValue.length != width)
       throw new IllegalArgumentException("hashValue has incorrect size.");
    if (leq(group_order, hashValue)) sub(hashValue, hashValue, group_order); 

    // Check emphemeral key
    if (ephemeralKey == null) throw new NullPointerException("ephemeralKey");
    if (ephemeralKey.length != width)
       throw new IllegalArgumentException("Unexpected ephemeral key size.");
    if (is_zero(ephemeralKey))
      throw new IllegalArgumentException("ephemeralKey is zero.");
    if (leq(group_order, ephemeralKey))
      throw new IllegalArgumentException("ephemeralKey must be less than group order.");

    //ec_mul(rP, ephemeralKey, basePoint);
    ec_mul_window(rP, ephemeralKey, basePoint, basePoint3, basePoint5);

    int[] r = signature.r;

    // Get affine coordinate for rP.x
    mod_div(r, field_unit, rP.z, field_prime); // rX = 1 / rP.z
    field_sq(r, r); // r = 1 / rP.z^2
    field_mul(r, rP.x, r); // r = rP.x / rP.z^2

    // Subtract group_order from rX if needed (should be rare).
    if (leq(group_order, r)) sub(r, r, group_order);

    // Fail if the r coordinate is zero (should be rare)
    if (is_zero(signature.r)) {
      cleanup();
      return false;
    }

    group_mul(h, privateKey, signature.r);
    group_add(h, h, hashValue);

    // Let signature.s = (e + d * sig_r) / ephemeralKey (mod group_order)
    mod_div(signature.s, h, ephemeralKey, group_order);
    boolean failed = is_zero(signature.s);
    cleanup();
    return !failed;
  }

  /**
   * Create a fresh public key for this curve.
   */
  public PublicKey createPublicKey() {
    return new PublicKey(width);
  }

  /**
   * Create a fresh uninitialized signature for this curve.
   */
  public Signature createSignature() {
    return new Signature(width);
  }

  /**
   * Initialize public key for this curve using given private key.
   *
   * @param publicKey Public key to assign to.
   * @param privateKey Private key to use for generating public key.
   *
   * @throws NullPointerException Thrown if a parameter is <code>null</code>.
   * @throws IllegalArgumentException Thrown if a parameter has an incorrect size.
   */
  public void initializePublicKey(PublicKey publicKey, int[] privateKey) {
    if (publicKey == null) throw new NullPointerException("publicKey");
    if (publicKey.x.length != width)
       throw new IllegalArgumentException("Unexpected public key size.");

    if (privateKey == null) throw new NullPointerException("privateKey");
    if (privateKey.length != width)
       throw new IllegalArgumentException("privateKey has incorrect size.");
    if (is_zero(privateKey))
      throw new IllegalArgumentException("privateKey is zero.");
    if (leq(group_order, privateKey))
      throw new IllegalArgumentException("privateKey must be less than group order.");

    ec_mul_window(rP, privateKey, basePoint, basePoint3, basePoint5);

    mod_div(publicKey.x, field_unit, rP.z, field_prime); // publicKey.x = 1 / rP.z
    field_mul(publicKey.y, rP.y, publicKey.x); // publicKey.y = rP.y / rP.z
    field_sq(publicKey.x, publicKey.x); // publicKey.x = 1 / rP.z^2
    field_mul(publicKey.y, publicKey.y, publicKey.x); // publicKey.y = rP.y / rP.z^3
    field_mul(publicKey.x, publicKey.x, rP.x); // publicKey.x = rP.x / rP.z^2
    cleanup();
  }

  /**
   * Attempts to verify signature, returns true if successful.
   *
   * @param hashValue Hash value generated by hash function, and converted as specified in appendix A.5.
   * @param signature Signature to verify.
   * @param publicKey Public key of signatory.
   *
   * @return <code>true</code> if the signature was verified.
   * @throws NullPointerException Thrown if one of the parameters is <code>null</code>.
   * @throws IllegalArgumentException Thrown if the signature or public key were not generated for
   *         this curve.
   */
  public boolean verifySignature(int[] hashValue, Signature signature, PublicKey publicKey) {
    if (hashValue == null) throw new NullPointerException("hashValue");
    if (hashValue.length != width)
       throw new IllegalArgumentException("hashValue has incorrect size.");
    if (leq(group_order, hashValue)) sub(hashValue, hashValue, group_order); 

    if (signature == null) throw new NullPointerException("signature");
    if (signature.r.length != width)
       throw new IllegalArgumentException("Unexpected signature size.");

    if (publicKey == null) throw new NullPointerException("publicKey");
    if (publicKey.x.length != width)
       throw new IllegalArgumentException("Unexpected public key size.");

    if (is_zero(signature.r) || !leq(signature.r, group_order)) return false;
    if (is_zero(signature.s) || !leq(signature.s, group_order)) return false;

    mod_div(h, field_unit, signature.s, group_order); // h = 1 / s
    group_mul(u1, hashValue, h); // u1 <- hashValue / s
    group_mul(u2, signature.r, h);         // u2 <- signature.r / s

    assign(qPoint.x, publicKey.x);
    assign(qPoint.y, publicKey.y);
    ec_twin_mul(rP, u1, basePoint, u2, qPoint, sPtP, sMtP, sPt, sMt);

    if (is_zero(rP.z)) return false;

    // rP.z = rP.z * 2
    field_sq(rP.z, rP.z);

    field_mul(t1, signature.r, rP.z);
    if (is_equal(t1, rP.x)) {
      cleanup();
      return true;
    }

    if (add(h, signature.r, group_order) == 0) {
      field_mul(t1, h, rP.z);
      if (is_equal(t1, rP.x)) {
        cleanup();
        return true;
      }
    }

    cleanup();
    return false;
  }
  // }}}2
}
