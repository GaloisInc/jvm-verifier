import com.galois.symbolic.*;
import com.galois.ecc.*;

public class FieldRedAux
{
  public static void main(String[] args) throws Exception
  {
    /*    int[] z = Symbolic.freshIntArray(12);
    int[] a = Symbolic.freshIntArray(24);
    */
    int[] z = new int[12];
    int[] a = new int[24];
    for (int i = 0; i != 12; ++i) z[i] = 0;
    for (int i = 0; i != 24; ++i) a[i] = 0;
    a[23] = 0x80000000;

    ECCProvider p = NISTCurveFactory.createP384_64();
    System.out.println("Driver begin.");
    int d = p.field_red_aux(z, a);
    System.out.println("d = 0x" + String.format("%x", d));
  }
}
