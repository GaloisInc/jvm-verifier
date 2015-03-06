import com.galois.symbolic.*;
class ExampleCNF {
    public static void main(String[] args) {
        byte x = Symbolic.freshByte((byte)0);
        // To prove an equality with SAT we check that its negation is
        // unsatisfiable.
        Symbolic.writeCnf("ExampleCNF.cnf", ! (x + x == 2 * x));
    }
}
