package tys.frontier.util;

public class IntPair<T> {

    public T a;
    public int i;

    public IntPair(T a, int i) {
        this.a = a;
        this.i = i;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IntPair<?> intPair = (IntPair<?>) o;

        return  i == intPair.i &&  a != null ? a.equals(intPair.a) : intPair.a == null;
    }

    @Override
    public int hashCode() {
        int result = a != null ? a.hashCode() : 0;
        result = 31 * result + i;
        return result;
    }
}
