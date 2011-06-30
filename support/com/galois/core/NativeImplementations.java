package Galois;

public final class NativeImplementations {
  private NativeImplementations() {}

  public static void arraycopy(Object src,
                               int srcPos,
                               Object dest,
                               int destPos,
                               int length) {
    if (dest == null)
      throw new NullPointerException();
    if (src == null)
      throw new NullPointerException();

    if (src instanceof boolean[]) {
      if (!(dest instanceof boolean[])) {
         throw new ArrayStoreException();
      }

      boolean[] aSrc  = (boolean[]) src;
      boolean[] aDest = (boolean[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        boolean[] temp = new boolean[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof byte[]) {
      if (!(dest instanceof byte[])) {
         throw new ArrayStoreException();
      }

      byte[] aSrc  = (byte[]) src;
      byte[] aDest = (byte[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        byte[] temp = new byte[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof char[]) {
      if (!(dest instanceof char[])) {
         throw new ArrayStoreException();
      }

      char[] aSrc  = (char[]) src;
      char[] aDest = (char[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        char[] temp = new char[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }
    
    } else if (src instanceof int[]) {
      if (!(dest instanceof int[])) {
         throw new ArrayStoreException();
      }

      int[] aSrc  = (int[]) src;
      int[] aDest = (int[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        int[] temp = new int[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof long[]) {
      if (!(dest instanceof long[])) {
         throw new ArrayStoreException();
      }

      long[] aSrc  = (long[]) src;
      long[] aDest = (long[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        long[] temp = new long[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof short[]) {
      if (!(dest instanceof short[])) {
         throw new ArrayStoreException();
      }

      short[] aSrc  = (short[]) src;
      short[] aDest = (short[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        short[] temp = new short[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof double[]) {
      if (!(dest instanceof double[])) {
         throw new ArrayStoreException();
      }

      double[] aSrc  = (double[]) src;
      double[] aDest = (double[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        double[] temp = new double[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof float[]) {
      if (!(dest instanceof float[])) {
         throw new ArrayStoreException();
      }

      float[] aSrc  = (float[]) src;
      float[] aDest = (float[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        float[] temp = new float[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else if (src instanceof Object[]) {
      if (!(dest instanceof Object[])) {
         throw new ArrayStoreException();
      }

      Object[] aSrc  = (Object[]) src;
      Object[] aDest = (Object[]) dest;

      if (   (srcPos  < 0) 
          || (destPos < 0) 
          || (length  < 0)
          || (srcPos  > aSrc.length - length)
          || (destPos > aDest.length - length)) {
        throw new IndexOutOfBoundsException();
      }

      if (aSrc == aDest) {
        Object[] temp = new Object[length];
        for (int i = 0; i != length; ++i) {
          temp[i] = aSrc[srcPos + i];
        }
        for (int i = 0; i != length; ++i) {
            aDest[destPos + i] = temp[i];
        }
      } else {
        for (int i = 0; i != length; ++i) {
          aDest[destPos + i] = aSrc[srcPos + i]; 
        }
      }

    } else {
      throw new ArrayStoreException();
    }
  }
}
