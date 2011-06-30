public class AesTest {

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

  public static void printReverseIntArray(int[] output) {
    for (int i = output.length; i != 0; --i) {
      System.out.printf("%08x ", output[i-1]);
    }
  }


  public static void printCryptol(String key, String input) {
    int[] output = new int[4];
    AESCryptol.run(reverseIntFromString(key),
                   reverseIntFromString(input),
                   output);
    printReverseIntArray(output);
    System.out.println();
  }
      

  public static void printCryptolNoTables(String key, String input) {
    int[] output = new int[4];
    AESCryptolNoTables.run(reverseIntFromString(key),
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


    // Cryptol
    printCryptol(key, input);
    printCryptolNoTables(key, input);

    printCryptol(key1, input1);
    printCryptolNoTables(key1, input1);
  }
}
