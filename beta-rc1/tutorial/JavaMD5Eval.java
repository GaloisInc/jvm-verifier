import org.bouncycastle.crypto.digests.*;
import com.galois.symbolic.*;

/**
   Evaluate the AIG for the symbolic simulation of MD5 on an
   specific 16-byte input message.
 */

class JavaMD5Eval
{
    public static void main(String[] args) throws Exception
    {
        byte[] msg = Symbolic.freshByteArray(16);
        byte[] out = new byte[16];
        MD5Digest digest = new MD5Digest();

        digest.update(msg, 0, msg.length);
        digest.doFinal(out, 0);
        byte[] result = Symbolic.evalAig(out,
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
            new CValue.CByte((byte) 0x21), // !
            new CValue.CByte((byte) 0x21), // !
            new CValue.CByte((byte) 0x21), // !
            new CValue.CByte((byte) 0x21), // !
            new CValue.CByte((byte) 0x21)  // !
          });
        for(int i = 0; i < result.length; i++) {
          System.out.println(result[i]);
        }
    }
}
