package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FIntN;

public class FInt32Literal implements FLiteral {

    public final int value;
    public final String originalString;

    public FInt32Literal(int value, String originalString) {
        this.value = value;
        this.originalString = originalString;
    }

    @Override
    public String getOriginalString() {
        return originalString;
    }

    @Override
    public FClass getType() {
        return FIntN._32;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FInt32Literal that = (FInt32Literal) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return value;
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
