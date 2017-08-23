package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FInt64;

public class FInt64Literal implements FLiteral {

    public final long value;
    public final String originalString;

    public FInt64Literal(long value, String originalString) {
        this.value = value;
        this.originalString = originalString;
    }

    @Override
    public FClass getType() {
        return FInt64.INSTANCE;
    }

    @Override
    public String getOriginalString() {
        return originalString;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FInt64Literal that = (FInt64Literal) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return (int) (value ^ (value >>> 32));
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
