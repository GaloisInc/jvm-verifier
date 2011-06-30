/*
Module           : TestRC564.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/


import org.bouncycastle.crypto.engines.*;
import org.bouncycastle.crypto.params.*;
import org.bouncycastle.crypto.*;

class TestRC564
{
    public static byte[] byteFromString(String value) {
        byte[] result = new byte[value.length()/2];
        for (int i = 0; i < result.length; ++i) {
            result[i] = (byte) Integer.parseInt(value.substring(2*i, 2*i+2), 16);
        }
        return result;
    }
  
    public static void rc564_encrypt(byte[] key, byte[] pt, byte[] output) 
    {
        BlockCipher engine   = new RC564Engine();
        RC5Parameters params = new RC5Parameters(key, 18);
        engine.init(true, params);
        engine.processBlock(key, 0, output, 0);
    }

    public static void main(String[] args) 
    {
        if (args.length > 2 || args.length < 2)
        {
            System.out.println("Invalid #arguments");
            System.exit(1);
        }
        byte[] key    = byteFromString(args[0]);
        byte[] pt     = byteFromString(args[1]);
        byte[] output = new byte[16];
        rc564_encrypt(key, pt, output);

        for (int i = 0; i != output.length; ++i) 
            System.out.printf("%02x", output[i]);
    }
}
