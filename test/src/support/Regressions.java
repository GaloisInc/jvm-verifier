import com.galois.symbolic.*;

class Regressions {

    public static void one_armed_if() {
	int n;
	int i = Symbolic.freshInt(0);
	if(i == 0) { n = 0; }
    }

    /*
     * use a String so that its class initializer runs; regression
     * test for a161eb3efdf4cba716d1a089c810267c47d2492b
     */
    public static void use_string() {
        String foo = "foo";
        return;
    }

}
