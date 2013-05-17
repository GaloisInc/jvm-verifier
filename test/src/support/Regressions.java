import com.galois.symbolic.*;

class Regressions {

    public static void one_armed_if() {
	int n;
	int i = Symbolic.freshInt(0);
	if(i == 0) { n = 0; }
    }

}
