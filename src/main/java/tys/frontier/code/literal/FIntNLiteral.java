package tys.frontier.code.literal;

import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FClass;

import java.math.BigInteger;

public class FIntNLiteral implements FLiteral {

    public final BigInteger value;
    private FIntN type;
    public final String originalString;

    public FIntNLiteral(long value, String originalString) {
        this(BigInteger.valueOf(value), originalString);
    }

    public FIntNLiteral(long value) {
        this(value, "" + value);
    }

    public FIntNLiteral(BigInteger value) {
        this(value, value.toString());
    }

    public FIntNLiteral(BigInteger value, String originalString) {
        this.value = value;
        this.type = FIntN.getIntN(FIntN.neededBits(value));
        this.originalString = originalString;
    }

    private FIntNLiteral(BigInteger value, FIntN type, String originalString) {
        this.value = value;
        this.type = type;
        this.originalString = originalString;
    }

    @Override
    public FIntNLiteral copy() {
        return new FIntNLiteral(value, type, originalString);
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
