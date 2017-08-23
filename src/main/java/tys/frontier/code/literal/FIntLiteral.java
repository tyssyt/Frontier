package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FInt;

import java.math.BigInteger;

public class FIntLiteral implements FLiteral {

    public final BigInteger value;
    public final String originalString;

    public FIntLiteral(BigInteger value, String originalString) {
        this.value = value;
        this.originalString = originalString;
    }

    public FIntLiteral(long value, String originalString) {
        this.value = BigInteger.valueOf(value);
        this.originalString = originalString;
    }

    @Override
    public FClass getType() {
        return FInt.INSTANCE;
    }

    @Override
    public String getOriginalString() {
        return originalString;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FIntLiteral that = (FIntLiteral) o;

        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
