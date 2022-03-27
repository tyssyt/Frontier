package tys.frontier.code.identifier;

import tys.frontier.util.Utils;

public class FIdentifier implements Comparable<FIdentifier>, IdentifierNameable {

    public static final FIdentifier THIS = new FIdentifier("!this");

    public static final FIdentifier BOOL = new FIdentifier("!Bool");
    public static final FIdentifier FLOAT32 = new FIdentifier("!Float32");
    public static final FIdentifier FLOAT64 = new FIdentifier("!Float64");
    public static final FIdentifier VOID = new FIdentifier("!Void");

    public final String name;

    public FIdentifier(String name) {
        this.name = Utils.removeLeadingUnderscores(name);
    }

    @Override
    public FIdentifier getIdentifier() {
        return this;
    }

    @Override
    public int compareTo(FIdentifier o) {
        return name.compareTo(o.name);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FIdentifier that = (FIdentifier) o;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name;
    }
}
