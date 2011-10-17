class LocalSpec {
    public static void test1(int[] a) {
        a[0] = 1;
        // arbitrary: a
        a[1] = 2;
        // arbitrary: a
    }

    public static void test2(int[] a) {
        a[0] = 1;
        // ensures a == 1;
        a[0]++;
        // ensures a == 2;
    }

    public static void test3(int[] a, int[] b) {
        for(a[0] = 0; a[0] < b[0]; a[0]++) {
        }
    }

    public static void copy(int[] a, int[] b, int[] i) {
        for(i[0] = 0; i[0] < 12; i[0]++) {
            // assume 0 <= i[0]
            b[i[0]] = a[i[0]];
            // ensures b[i[0]] := a[i[0]]
        }
        /* This leads to two verification conditions:
         *   1) (0 <= i && i < 12) ==> b[i] == a[i]
         *   2) i > 12 ==> true
         * which boils down to just (1).
         */
    }

    public static int fac(int[] i, int[] a) {
        a[0] = 1;
        while(i[0] != 0) {
            a[0] = a[0] * i[0];
            i[0]--;
        }
        return a[0];
    }
}
