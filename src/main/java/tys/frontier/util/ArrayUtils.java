package tys.frontier.util;

import java.util.Arrays;

public class ArrayUtils {

    private ArrayUtils() {}

    public static int[] create(int size, int fill) {
        int[] ints = new int[size];
        Arrays.fill(ints, fill);
        return ints;
    }

}
