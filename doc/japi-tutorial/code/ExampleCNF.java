import com.galois.symbolic.*;
class TestSat {
    public static void main(String[] args) {
        byte x = Symbolic.freshByte((byte)0);
        Symbolic.writeCnf("TestSat.cnf", x + x == 2 * x);
    }
}
