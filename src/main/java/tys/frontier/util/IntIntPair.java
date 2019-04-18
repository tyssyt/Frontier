package tys.frontier.util;

public class IntIntPair implements Comparable<IntIntPair> {

    public int a;
    public int b;

    public IntIntPair(int a, int b) {
        this.a = a;
        this.b = b;
    }

    @Override
    public int compareTo(IntIntPair o) {
        if (this.a != o.a)
            return this.a - o.a;
        return this.b - o.b;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IntIntPair)) return false;

        IntIntPair that = (IntIntPair) o;

        if (a != that.a) return false;
        return b == that.b;
    }

    @Override
    public int hashCode() {
        int result = a;
        result = 31 * result + b;
        return result;
    }
}
