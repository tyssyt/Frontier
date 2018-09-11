package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FIntN;

import java.math.BigInteger;

public class FIntNLiteral implements FLiteral {

    public final BigInteger value;
    public final FIntN type;
    public final String originalString;

    public FIntNLiteral(long value, int bitWidth, String originalString) {
        this(BigInteger.valueOf(value), bitWidth, originalString);
    }

    public FIntNLiteral(long value, int bitWidth) {
        this(value, bitWidth, "" + value);
    }

    public FIntNLiteral(BigInteger value, int bitWidth) {
        this(value, bitWidth, value.toString());
    }

    public FIntNLiteral(BigInteger value, int bitWidth, String originalString) {
        this.value = value;
        this.type = FIntN.getIntN(bitWidth);
        this.originalString = originalString;
    }

    @Override
    public String getOriginalString() {
        return originalString;
    }

    @Override
    public FClass getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FIntNLiteral)) return false;

        FIntNLiteral that = (FIntNLiteral) o;

        if (!value.equals(that.value)) return false;
        if (!type.equals(that.type)) return false;
        return originalString.equals(that.originalString);
    }

    @Override
    public int hashCode() {
        int result = value.hashCode();
        result = 31 * result + type.hashCode();
        result = 31 * result + originalString.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
