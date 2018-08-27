package tys.frontier.code.literal;

import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FIntN;

public class FCharLiteral implements FLiteral {

    public final char value;

    public FCharLiteral(char value) {
        this.value = value;
    }

    @Override
    public String getOriginalString() {
        return value+"";
    }

    @Override
    public FType getType() {
        return FIntN._8;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FCharLiteral)) return false;

        FCharLiteral that = (FCharLiteral) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return (int) value;
    }

    @Override
    public String toString() {
        return "" + '\'' + value + '\'';
    }
}
