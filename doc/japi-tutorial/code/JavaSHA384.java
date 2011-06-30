import org.bouncycastle.crypto.digests.*;
import com.galois.symbolic.*;

/**
   Writes out an AIG for the symbolic simulation of SHA-384 on an
   arbitrary 4-byte input message.
 */

class JavaSHA384
{
    public static void main(String[] args) throws Exception
    {
        byte[] msg = Symbolic.freshByteArray(4);
        byte[] out = new byte[48];
        SHA384Digest digest = new SHA384Digest();

        digest.update(msg, 0, msg.length);
        digest.doFinal(out, 0);
        Symbolic.writeAiger("JavaSHA384.aig", out);

    }
}
