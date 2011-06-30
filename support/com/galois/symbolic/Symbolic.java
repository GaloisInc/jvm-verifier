/*
Module           : Symbolic.java
Description      : The Java interface to JSS
Stability        : provisional
Point-of-contact : jstanley
*/

package com.galois.symbolic;

/** An interface to the Java Symbolic Simulator */
public class Symbolic
{
    /**
     * Provides a new symbolic byte term to the caller.  When invoked in
     * a standard JVM, returns its argument.
     *
     * @param dflt the default value to use for concrete execution
     */
    public static byte freshByte(byte dflt) { return dflt; }

    /**
     * Provides a new symbolic int term to the caller.  When invoked in
     * a standard JVM, returns its argument.
     *
     * @param dflt the default value to use for concrete execution
     */
    public static int freshInt(int dflt) { return dflt; }

    /**
     * Provides a new symbolic long term to the caller.  When invoked in
     * a standard JVM, returns its argument.
     *
     * @param dflt the default value to use for concrete execution
     */
    public static long freshLong(long dflt) { return dflt; }

    /**
     * Provides a new symbolic boolean term to the caller.  When invoked
     * in a standard JVM, returns its argument.
     *
     * @param dflt the default value to use for concrete execution
     */
    public static boolean freshBoolean(boolean dflt) { return dflt; }

    /**
     * Provides a new array of symbolic byte terms, of the specified
     * length.  When invoked in a standard JVM, returns
     * <code>null</code>.
     *
     * @param sz the number of elements in the new symbolic array
     */
    public static byte[] freshByteArray(int sz) { return null; }

    /**
     * Provides a new array of symbolic int terms, of the specified
     * length.  When invoked in a standard JVM, returns
     * <code>null</code>.
     *
     * @param sz the number of elements in the new symbolic array
     */
    public static int[] freshIntArray(int sz) { return null; }

    /**
     * Provides a new array of symbolic long terms, of the specified
     * length.  When invoked in a standard JVM, returns
     * <code>null</code>.
     *
     * @param sz the number of elements in the new symbolic array
     */
    public static long[] freshLongArray(int sz) { return null; }

    /**
     * Evaluate the simulation AIG at the byte output <code>out</code>
     * with concrete inputs given by the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static byte evalAig(byte out, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by symbolic simulator");
    }
    /**
     * Evaluate the simulation AIG at the int output <code>out</code>
     * with concrete inputs given by the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static int evalAig(int out, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by the symbolic simulator");
    }
    /**
     * Evaluate the simulation AIG at the long output <code>out</code>
     * with concrete inputs given by the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static long evalAig(long out, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by the symbolic simulator");
    }

    /** Evaluate the simulation AIG at byte outputs <code>outs</code>
     * with concrete inputs given via the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static byte[] evalAig(byte[] outs, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by the symbolic simulator");
    }
    /** Evaluate the simulation AIG at int outputs <code>outs</code>
     * with concrete inputs given via the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static int[] evalAig(int[] outs, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by the symbolic simulator");
    }
    /** Evaluate the simulation AIG at long outputs <code>outs</code>
     * with concrete inputs given via the <code>opds</code> array.  When
     * invoked in a standard JVM, throws an exception.
     */
    public static long[] evalAig(long[] outs, CValue[] opds) throws Exception
    {
        throw new Exception("evalAig not invoked by the symbolic simulator");
    }

    /** Write the AIG with the given boolean output to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param out the variable to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, boolean out) { return; }

    /** Write the AIG with the given byte output to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param out the variable to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, byte out)    { return; }

    /** Write the AIG with the given int output to the named file.  When
     * invoked in the context of the symbolic simulator, does nothing.
     *
     * @param fname the file name to store the AIG data
     * @param out the variable to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, int out)     { return; }

    /** Write the AIG with the given long output to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param out the variable to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, long out)    { return; }

    /** Write the AIG with the given byte outputs to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param outs the variables to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, byte[] outs) { return; }

    /** Write the AIG with the given int outputs to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param outs the variables to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, int[] outs)  { return; }

    /** Write the AIG with the given long outputs to the named file.
     * When invoked in the context of the symbolic simulator, does
     * nothing.
     *
     * @param fname the file name to store the AIG data
     * @param outs the variables to symbolically represent as an AIG file
     */
    public static void writeAiger(String fname, long[] outs) { return; }

    /**
     * When invoked in a standard JVM, this function always returns
     * null. When invoked in the context of the symbolic simulator,
     * function returns an array populated with (opaque) path state
     * descriptors for all paths (including those that have no
     * terminated!), subject to inclusion constraints. These values
     * can be used in tandem with other API functions to obtain
     * information incrementally about the simulation results of
     * particular paths.
     *
     * @param includeExc include descriptors for paths that have
     * terminated but have resulted in an exception.
     */
    public static int[] getPathDescriptors(boolean includeExc)
    {
        return null;
    }

    /** Debugging helpers for interaction with the Java Symbolic Simulator */
    public static class Debug
    {
        /**
         * Print an integer using the simulator's pretty printer.
         */
        public static void trace(int x) { return; }
        /**
         * Print a labeled integer using the simulator's pretty printer.
         */
        public static void trace(String label, int x) { return; }
        /**
         * Print a long using the simulator's pretty printer.
         */
        public static void trace(long x) { return; }
        /**
         * Print a labeled long using the simulator's pretty printer.
         */
        public static void trace(String label, long x) { return; }

        /**
         * Abort simulation on all paths and terminate. When invoked in
         * a standard JVM, this function does nothing.
         */
        public static void abort() { return; }

        /**
         * Print the set of all path states currently being simulated
         * are printed to stdout.  When invoked in a standard JVM, this
         * function does nothing.
         */
        public static void dumpPathStates() { return; }

        /**
         * Set debug verbosity to the requested level.  When invoked in
         * a standard JVM, this function does nothing.
         */
        public static void setVerbosity(int level) { return; }

//         /**
//          * When invoked in the context of the symbolic simulator,
//          * traverses the in-memory dag, evaluating terms and
//          * bit-blasting them in lockstep, checking for correctness.
//          * Function parameters and return values are as in the
//          * <code>Symbolic.evalAig()</code> functions.
//          */
//         public static int eval(int out, CValue[] opds) throws Exception
//         {
//             throw new Exception("eval not invoked by the symbolic simulator");
//         }
    }
}
