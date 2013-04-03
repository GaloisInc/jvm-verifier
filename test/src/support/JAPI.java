/*
Module           : JAPI.java
Description      : Test cases for the Java interface to JSS
Stability        : provisional
Point-of-contact : jstanley
*/

import com.galois.symbolic.*;

public class JAPI
{
    public static class BadTestNameException         extends Exception {}
    public static class NoTestNameException          extends Exception {}

    public static void main(String[] args) throws Exception
    {
        if (args.length != 1) throw new NoTestNameException();
        String testName = args[0];

        if (testName.equals("t2"))           { t2();           return; }
        if (testName.equals("t8"))           { t8();           return; }
        if (testName.equals("tarr1"))        { tarr1();        return; }
        if (testName.equals("tarr2"))        { tarr2();        return; }
        if (testName.equals("t9"))           { t9();           return; }
        if (testName.equals("simpleByte"))   { simpleByte();   return; }
        if (testName.equals("byteProd"))     { byteProd();     return; }
        if (testName.equals("outArr"))       { outArr();       return; }
        //if (testName.equals("dbugEvalTest")) { dbugEvalTest(); return; }

        throw new BadTestNameException();
    }

    public static void t2() throws Exception {
        int x_sym    = Symbolic.freshInt(42);
        int y_sym    = Symbolic.freshInt(99);
        int rslt_sym = x_sym + y_sym;

        int rslt = Symbolic.evalAig(
            rslt_sym,
            new CValue[] {
                new CValue.CInt(5),
                new CValue.CInt(7)
            });
        if (rslt != 12)
            throw new Exception("JAPI t2 failed");
        Symbolic.writeAiger("t2-japi.aig", rslt_sym);
    }

    public static void t8() throws Exception {
        long x_sym    = Symbolic.freshLong(42);
        long y_sym    = Symbolic.freshLong(99);
        long rslt_sym = x_sym + y_sym;

        long rslt = Symbolic.evalAig(
            rslt_sym,
            new CValue[] {
                new CValue.CLong(20),
                new CValue.CLong(29)
            });
        if (rslt != 49)
            throw new Exception("JAPI t8 failed");
        Symbolic.writeAiger("t8-japi.aig", rslt_sym);
    }

    public static void tarr1() throws Exception {
        int[] arr = Symbolic.freshIntArray(3);
        arr[0] = arr[1] + arr[2];

        int[] rslt = Symbolic.evalAig(
            arr,
            new CValue[] {
                new CValue.CInt(0),
                new CValue.CInt(2),
                new CValue.CInt(4)
            });
        if (rslt[0] != 6 || rslt[1] != 2 || rslt[2] != 4)
            throw new Exception("JAPI tarr1 failed");
        Symbolic.writeAiger("tarr1-japi.aig", arr);
        return;
    }

    public static void tarr2() throws Exception {
        long[] arr = Symbolic.freshLongArray(3);
        arr[2]     = arr[0] * arr[1];
        long[] rslt = Symbolic.evalAig(
            arr,
            new CValue[] {
                new CValue.CLong(2),
                new CValue.CLong(16),
                new CValue.CLong(0)
            });
        if (rslt[0] != 2 || rslt[1] != 16 || rslt[2] != 32)
            throw new Exception("JAPI tarr2 failed");
        Symbolic.writeAiger("tarr2-japi.aig", arr);
    }

    public static void t9() throws Exception {
        long[] arr = Symbolic.freshLongArray(4);
        long out   = 0;
        for (int i = 0; i < arr.length; ++i)
            out += arr[i];
        long rslt  = Symbolic.evalAig(
            out,
            new CValue[] {
                new CValue.CLong(2210),
                new CValue.CLong(2313),
                new CValue.CLong(5595),
                new CValue.CLong(8675),
            });
        if (rslt != 18793)
            throw new Exception("JAPI t9 failed");
        Symbolic.writeAiger("t9-japi.aig", out);
    }

    public static void simpleByte() throws Exception {
        byte sb1  = Symbolic.freshByte((byte)99);
        byte sb2  = Symbolic.freshByte((byte)99);
        byte out  = (byte) (sb1 + sb2);
        byte rslt = Symbolic.evalAig(
            out,
            new CValue[] {
                new CValue.CByte((byte)63),
                new CValue.CByte((byte)64)
            });
        if (rslt != 127)
            throw new Exception("JAPI simplebyte failed");
        Symbolic.writeAiger("simplebyte-japi.aig", out);
    }

    public static void byteProd() throws Exception {
        byte[] arr = Symbolic.freshByteArray(4);
        byte out   = 1;
        for (int i = 0; i < arr.length; ++i)
            out *= arr[i];
        byte rslt  = Symbolic.evalAig(
            out,
            new CValue[] {
                new CValue.CByte((byte)2),
                new CValue.CByte((byte)3),
                new CValue.CByte((byte)4),
                new CValue.CByte((byte)5),
            });
        if (rslt != 120)
            throw new Exception("JAPI byteprod failed");
        Symbolic.writeAiger("byteprod-japi.aig", out);
    }

    public static void outArr() throws Exception {
        int[] outs = new int[2];
        outArrHelper(outs, Symbolic.freshInt(99), Symbolic.freshInt(99));
        int[] rslt = Symbolic.evalAig(
            outs,
            new CValue[] {
                new CValue.CInt(4),
                new CValue.CInt(4)
            });
        if (rslt[0] != 8 || rslt[1] != 16)
            throw new Exception("JAPI outarr failed");
        Symbolic.writeAiger("outarr-japi.aig", outs);
    }

    static void outArrHelper(int[] outArr, int x, int y) 
    {
        outArr[0] = x + y;
        outArr[1] = x * y;
    }

    /*
    public static void dbugEvalTest() throws Exception {
        int[] pi = new int[2];
        for (int i = 0; i < pi.length; ++i)
            pi[i] = Symbolic.freshInt(0);

        int po = pi[0] + pi[1];
                
        int rslt = Symbolic.Debug.eval(
            po,
            new CValue[] {
                new CValue.CInt(5),
                new CValue.CInt(7)
            });
        if (rslt != 12)
            throw new Exception("dbugEvalTest failed");
        //Symbolic.writeAiger("t2-japi.aig", rslt_sym);
        System.out.println("dbugEvalTest OK.");
    }
    */

}
