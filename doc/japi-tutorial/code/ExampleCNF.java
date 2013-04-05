import com.galois.symbolic.*;
class ExampleCNF {
    public static void main(String[] args) {
        byte x = Symbolic.freshByte((byte)0);
        Symbolic.writeCnf("ExampleCNF.cnf", x + x == 2 * x);
    }
}
