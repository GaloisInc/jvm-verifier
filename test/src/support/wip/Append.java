/*
See also:
test/ashes/ashesSuiteCollection/suites/ashesEasyTestSuite/benchmarks/factorial/src/Main.java
*/
class Append {
    /* String concatenation only works for some types. Errors in
     * StringBuilder seem to be responsible. */
    public static void main(String[] args) {
        System.out.println("foo" + 5);
        System.out.println("foo" + new Integer(5));
    }
}
