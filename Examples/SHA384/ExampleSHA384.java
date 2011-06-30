/*
Module           : ExampleSHA384.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/

import org.bouncycastle.crypto.digests.*;
import com.galois.symbolic.*;

/**
   Writes out an AIG for the symbolic simulation of SHA-384 on an
   arbitrary 11-byte input message.
 */

class ExampleSHA384
{
    public static void main(String[] args) throws Exception
    {
        byte[] msg          = Symbolic.freshByteArray(11);
        byte[] out          = new byte[48];
        SHA384Digest digest = new SHA384Digest();

        digest.update(msg, 0, msg.length);
        digest.doFinal(out, 0);

        byte[] rslt = Symbolic.evalAig(
            out,
            new CValue[] {
                new CValue.CByte((byte) 0x68), // h
                new CValue.CByte((byte) 0x65), // e
                new CValue.CByte((byte) 0x6c), // l
                new CValue.CByte((byte) 0x6c), // l
                new CValue.CByte((byte) 0x6f), // o
                new CValue.CByte((byte) 0x20), // 
                new CValue.CByte((byte) 0x77), // w
                new CValue.CByte((byte) 0x6f), // o
                new CValue.CByte((byte) 0x72), // r
                new CValue.CByte((byte) 0x6c), // l
                new CValue.CByte((byte) 0x64), // d
            });

        for (int i = 0; i < 48; ++i) {
            System.out.print(rslt[i]);
            System.out.print("\n");
        }
    }
}