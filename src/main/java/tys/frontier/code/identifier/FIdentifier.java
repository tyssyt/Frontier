package tys.frontier.code.identifier;

import tys.frontier.util.Utils;

public abstract class FIdentifier {
    public final String name;

    protected FIdentifier(String name) {
        this.name = Utils.removeLeadingUnderscores(name);
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
