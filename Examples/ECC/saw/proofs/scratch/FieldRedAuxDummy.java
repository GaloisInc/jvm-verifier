import com.galois.symbolic.*;
import com.galois.ecc.*;

public class FieldRedAuxDummy
{
  public static void main(String[] args) throws Exception
  {
    int[] z = new int[12];
    int[] a = new int[24];

    a[0] =  0xfffe0005 ;
    a[1] =  0xffffffff ;
    a[2] =  0x00007fff ;
    a[3] =  0xffffffff ;
    a[4] =  0xffffffff ;
    a[5] =  0xfffffff8 ;
    a[6] =  0xfffe0004 ;
    a[7] =  0xffffffff ;
    a[8] =  0xffffffff ;
    a[9] =  0xffffffff ;
    a[10] = 0xffffffff ;
    a[11] = 0xffffffff ;
    a[12] = 0xfffffff8 ;
    a[13] = 0x00000000 ;
    a[14] = 0x00000000 ;
    a[15] = 0xffffffff ;
    a[16] = 0xffffffff ;
    a[17] = 0x00000000 ;
    a[18] = 0xffffffff ;
    a[19] = 0xffffffff ;
    a[20] = 0x00000000 ;
    a[21] = 0x00000004 ;
    a[22] = 0x00000000 ;
    a[23] = 0x00000001 ;

    ECCProvider p = NISTCurveFactory.createP384_64();
    System.out.println("Driver begin.");

    System.out.println("Inputs:");
    for (int i = 0; i != 24; ++i)
      System.out.println("a[" + i + "] = 0x" + String.format("%x", a[i]));
    
    p.field_red(z,a);
    System.out.print("z = [");
    for(int i = 0; i != 12; ++i)
      System.out.print(" 0x" + String.format("%x(%d)", z[i], z[i]));
    System.out.println(" ] ");
  }
}
