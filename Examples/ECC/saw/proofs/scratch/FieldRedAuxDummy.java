import com.galois.symbolic.*;
import com.galois.ecc.*;

public class FieldRedAuxDummy
{
  public static void main(String[] args) throws Exception
  {
    int[] z_orig = new int[12];
    int[] z_new  = new int[12];
    int[] a = new int[24];

    for (int i = 0; i != 24; ++i) a[i] = 0;
    a[0] =        1624929387;
    a[1] =       -1445316183;
    a[2] =       -1937033947;
    a[3] =        1959494010;
    a[4] =        1431245496;
    a[5] =        -740231100;
    a[6] =         496366788;
    a[7] =       -2127222880;
    a[8] =       -1750374719;
    a[9] =        1587831223;
    a[10] =        -25984125;
    a[11] =        155515770;
    a[12] =      -1983411246;
    a[13] =      -1648068199;
    a[14] =      -1538680812;
    a[15] =       1678235953;
    a[16] =      -1159409951;
    a[17] =       -966204029;
    a[18] =      -1294930512;
    a[19] =       1153431254;
    a[20] =       1403535450;
    a[21] =       -676333953;
    a[22] =       -560125294;
    a[23] =        470855587;

    ECCProvider p = NISTCurveFactory.createP384_64();
    System.out.println("Driver begin.");

    System.out.println("Inputs:");
    for (int i = 0; i != 24; ++i)
      System.out.println("a[" + i + "] = 0x" + String.format("%x", a[i]));
    
    p.field_red_orig(z_orig,a);
    System.out.print("z(orig) [");
    for(int i = 0; i != 12; ++i)
      System.out.print(" 0x" + String.format("%x", z_orig[i]));
    System.out.println(" ] ");

    p.field_red(z_new,a);
    System.out.print("z( new) [");
    for(int i = 0; i != 12; ++i)
      System.out.print(" 0x" + String.format("%x", z_new[i]));
    System.out.println(" ] ");

    for(int i = 0; i != 12; ++i) {
      if(z_orig[i] != z_new[i]) {
        System.out.println("Orig and new do not match :(");
        return;
      }
    }
    System.out.println("Orig and new match. Yay!");
  }
}
