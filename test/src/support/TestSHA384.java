/*
Module           : TestSHA384.java
Description      :
Stability        : provisional
Point-of-contact : jstanley
*/

// Hello world: Run manually with: java -classpath "../jdk1.6:." TestSHA384 "68656c6c6f20776f726c64"
// Expected output: fdbd8e75a67f29f701a4e040385e2e23986303ea10239211af907fcbb83578b3e417cb71ce646efd0819dd8c088de1bd

import org.bouncycastle.crypto.digests.*;

class TestSHA384
{
    public static byte[] byteFromString(String value) {
        byte[] result = new byte[value.length()/2];
        for (int i = 0; i < result.length; ++i) {
            result[i] = (byte) Integer.parseInt(value.substring(2*i, 2*i+2), 16);
        }
        return result;
    }

    public static void sha384_digest(byte[] message, byte[] output)
    {
        SHA384Digest digest = new SHA384Digest();
        digest.update(message, 0, message.length);
        digest.doFinal(output, 0);
    }

    public static void main(String[] args) 
    {
        if (args.length > 1 || args.length < 1) {
            System.out.println("Invalid #arguments");
            System.exit(1);
        }

        byte[] message = byteFromString(args[0]);
        byte[] output  = new byte[48];
        sha384_digest(message, output);

        for (int i = 0; i != output.length; ++i) 
            System.out.printf("%02x", output[i]);
    }
}
