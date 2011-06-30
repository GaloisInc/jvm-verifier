/*
Module           : PathStateMerges.java
Description      : Tests for path merging behavior; some prolematic cases too.
Stability        : provisional
Point-of-contact : jstanley
*/


import com.galois.symbolic.*;

// Tests for data dependent branches and path state merging

public class PathStateMerges
{
//     public static void main(String[] args) throws Exception
//     {
//         boolean b = Symbolic.freshBoolean(false);
//         int[] arr = Symbolic.freshIntArray(2);
//         ddb1(b, arr);

//         // NB: Until we get merging to occur before this point (i.e.,
//         // immediately after a call), the following abort() will always
//         // occur (because there will always be exactly two distinct
//         // paths at this point).
//         int[] pds = Symbolic.getPathDescriptors(false);
//         System.out.println(pds.length);
//         if (pds.length != 1)
//             Symbolic.Debug.abort();

//         System.out.println("Success.");
//     }


    public static void ddb1(boolean b, int arr[]) 
    {
        if (b)
        {
            arr[0] = 42;
            arr[1] = 1000;
        }
        else
        {
            arr[0] = 99;
            arr[1] = 2000;
        }
        //        System.out.println("At end of ddb1, dumping states:");
        //        Symbolic.Debug.dumpPathStates();
    }

    public static int ddb2(boolean b) 
    {
        if (b)
            return 42;
        else
            return 99;
    }

    public static class Dummy
    {
        Dummy(int x) { m_x = x; }
        public int m_x;
        public static long m_st;

        static
        {
            m_st = 7;
        }
    }

    public static void ddb3(boolean b, Dummy[] dummies)
    {
        Dummy d = new Dummy(42);
        if (b) 
            dummies[0] = d;
        else {
            dummies[0] = d;
            System.out.println("(dummy output)");
        }

/* Should and does fail: 
        if (b) 
            dummies[0] = new Dummy(42);
        else
            dummies[0] = new Dummy(99);
*/
    }

    public static void ddb4(boolean b, Dummy d)
    {
        if (b)
            d.m_x = 42;
        else
            d.m_x = 99;
    }

    public static void ddb5(boolean b, Dummy d)
    {
        if (b)
            d.m_st = 42;
        else
            return;
    }

    public static int ddb6(boolean b)
    {
        int x = ddb6_2(b);
        return x;
    }

    public static int ddb6_2(boolean b)
    {
        int x = ddb2(b);
        return x;
    }

    public static int ddb7(boolean b1, boolean b2)
    {
        int rslt = 0;
        for (int i = 0; i < 2; ++i)
            rslt += ddb7_h(b1,b2);
        return rslt;
    }

    public static int ddb7_h(boolean b1, boolean b2)
    {
        if (b1) {
            if (b2) {
                return 42;
            }
            else {
                return 1000;
            }
        }
        else {
            if (b2) {
                return 99;
            }
            else {
                return 2000;
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Problematic functions

    // ddb8 does not symbolically terminate.  Cryptol can't handle this either;
    // note that the ite is over a /symbolic/ x!
    public static int ddb8(int x) 
    {
        int j = 4;
        Symbolic.Debug.trace("j @ start", j);
        // Symbolic.Debug.setVerbosity(5);
        while (j >= 1) {
            j = ddb8_h(x, j);
            Symbolic.Debug.trace("j @ iteration end", j);
        }
        return j;
    }

    public static int ddb8_h(int x, int j) 
    {
        if (x == 42) {
            j--;
        }
        else {
            j -= 2; // NB: If this is changed to j -= 1, we terminate as
                    // expected because both branches yield the same thing and
                    // the Symbolic implementation accounts for this.
        }
        return j;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Problematic functions from the customer visit on 01 Dec 2010.

    public static int mul1(int x, int y)
    {
        if (x == 0)
            return 0;

        // NB: This function will not symbolically terminate until we can
        // 'optimize' it with code motion & CSE sufficient to make it look like
        // mul2 below.  It's even a little bit more subtle than that if one
        // looks at the Java bytecode for the below.

        if ((x & 1) == 1)
            return mul1(x >>> 1, y << 1) + y;
        else
            return mul1(x >>> 1, y << 1);
    }
    
    public static int mul2(int x, int y)
    {
        if (x == 0)
            return 0;

        // NB: The use of the logical shr operator here is vital for symbolic
        // termination!

        int z = mul2(x >>> 1, y << 1);

        if ((x & 1) == 1)
            return z + y;
        else
            return z;
    }
    
    public static int mul3(int x, int y, int d)
    {
//         Symbolic.Debug.trace("d", d);
//         Symbolic.Debug.trace("x", x);
//         Symbolic.Debug.trace("y", y);

        if (d == 0) return 0;
        if (x == 0) return 0;
        int z = mul3(x >> 1, y << 1, d - 1);
        return z + y * (x & 1);
    }

    public static void main(String[] args)
    {
        int a = Symbolic.freshInt(2);
        int b = Symbolic.freshInt(3);
        int out = PathStateMerges.mul2(a,b); 
//         Symbolic.Debug.trace("out", out);
        Symbolic.writeAiger("prob.aig", out);
    }
}
