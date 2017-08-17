package tys.frontier.util;

public class Triple<A, B, C> {
    public A a;
    public B b;
    public C c;

    public Triple() {
    }

    public Triple(A a, B b, C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Triple<?, ?, ?> triple = (Triple<?, ?, ?>) o;

        if (a != null ? !a.equals(triple.a) : triple.a != null) return false;
        if (b != null ? !b.equals(triple.b) : triple.b != null) return false;
        return c != null ? c.equals(triple.c) : triple.c == null;
    }

    @Override
    public int hashCode() {
        int result = a != null ? a.hashCode() : 0;
        result = 31 * result + (b != null ? b.hashCode() : 0);
        result = 31 * result + (c != null ? c.hashCode() : 0);
        return result;
    }
}
