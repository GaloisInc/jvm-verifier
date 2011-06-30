import org.bouncycastle.crypto.engines.*;
import org.bouncycastle.crypto.params.*;
import org.bouncycastle.crypto.*;

public class AesTest {

  public static byte[] byteFromString(String value) {
    byte[] result = new byte[value.length()/2];
    for (int i = 0; i < result.length; ++i) {
      result[i] = (byte) Integer.parseInt(value.substring(2*i, 2*i+2), 16);
    }
    return result;
  }

  public static int[] intFromString(String value) {
    int[] result = new int[value.length()/8];
    for (int i = 0; i < result.length; ++i) {
      int j = 8 * i;
      result[i] =  Integer.parseInt(value.substring(j+6, j+8), 16)
                + (Integer.parseInt(value.substring(j+4, j+6), 16) <<  8)
                + (Integer.parseInt(value.substring(j+2, j+4), 16) << 16)
                + (Integer.parseInt(value.substring(j+0, j+2), 16) << 24);
    }
    return result;
  }

  public static int[] reverseIntFromString(String value) {
    int[] result = new int[value.length()/8];
    for (int i = 0; i < result.length; ++i) {
      int j = 8 * (result.length - i - 1);
      result[i] =  Integer.parseInt(value.substring(j+6, j+8), 16)
                + (Integer.parseInt(value.substring(j+4, j+6), 16) <<  8)
                + (Integer.parseInt(value.substring(j+2, j+4), 16) << 16)
                + (Integer.parseInt(value.substring(j+0, j+2), 16) << 24);
    }
    return result;
  }


  public static void printIntArray(int[] output) {
    for (int i = 0; i != output.length; ++i) {
      System.out.printf("%08x ", output[i]);
    }
  }

  public static void printReverseIntArray(int[] output) {
    for (int i = output.length; i != 0; --i) {
      System.out.printf("%08x ", output[i-1]);
    }
  }

  public static void encrypt(BlockCipher engine, String key, String input) {
    KeyParameter param = new KeyParameter(byteFromString(key));

    byte[] output = new byte[16];
    engine.init(true, param);
    engine.processBlock(byteFromString(input), 0, output, 0);

    for (int i = 0; i != output.length; ++i) {
      System.out.printf("%02x", output[i]);
      if (i % 4 == 3)
        System.out.print(' ');
    }
    System.out.println();
  }

  public static void printCryptolAES(String key, String input) {
    int[] output = new int[4];
    AESCryptol.run(reverseIntFromString(key),
                   reverseIntFromString(input),
                   output);
    printReverseIntArray(output);
    System.out.println();
  }
      

  public static void main(String args[]) {
    String key   = "95A8EE8E89979B9EFDCBC6EB9797528D";
    String input = "4EC137A426DABF8AA0BEB8BC0C2B89D6";
    String key1   = "00000000000000000000000000000000";
    String input1 = "00000000000000000000000000000000";


    encrypt(new AESLightEngine(), key, input);
    encrypt(new AESEngine(), key, input);

    // Levant Cryptol C
    printIntArray(AES128Encrypt.encrypt(intFromString(key), intFromString(input)));
    System.out.println();

    // Cryptol
    printCryptolAES(key, input);

    System.out.println();
    System.out.println();
 
    encrypt(new AESLightEngine(), key1, input1);

    encrypt(new AESEngine(), key1, input1);

    printIntArray(AES128Encrypt.encrypt(intFromString(key1), intFromString(input1)));
    System.out.println();

    printCryptolAES(key1, input1);
  }
}
