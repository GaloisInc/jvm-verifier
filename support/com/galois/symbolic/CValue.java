/*
Module           : CValue.java
Description      : Concrete Java values
Stability        : provisional
Point-of-contact : jstanley
*/

package com.galois.symbolic;

/** The CValue class represents concrete byte, int, long, or boolean
 * values in a form usable as input to the Symbolic.evalAig methods. Because the
 * output variable given to Symbolic.evalAig may depend on arbitrary
 * input variables, this class needs to support a larger set of
 * potential values than the native {@link Byte}, {@link Integer},
 * {@link Long}, and {@link Boolean} classes, but a smaller set of
 * potential values than the {@link Object} class. */
public abstract class CValue
{
    /** A representation of a concrete byte */
    public static class CByte extends CValue
    {
        public CByte(byte value) { this.value = value; }
        public byte getValue() { return value; }
        protected byte value;
    }

    /** A representation of a concrete int */
    public static class CInt extends CValue
    {
        public CInt(int value) { this.value = value; }
        public int getValue() { return value; }
        protected int value;
    }

    /** A representation of a concrete long */
    public static class CLong extends CValue
    {
        public CLong(long value) { this.value = value; }
        public long getValue() { return value; }
        protected long value;
    }

    /** A representation of a concrete boolean */
    public static class CBool extends CValue
    {
        public CBool(boolean value) { this.value = value; }
        public boolean getValue() { return value; }
        protected boolean value;
    }
}
