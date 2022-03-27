package tys.frontier.code.literal;

import tys.frontier.code.predefinedClasses.FIntLiteralType;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FClass;

import java.math.BigInteger;

public class FIntNLiteral implements FLiteral {

    public static final FIntNLiteral INT32_0 = new FIntNLiteral(0,FIntN._32);

    public final BigInteger value;
    private FIntN type;

    public FIntNLiteral(long value, FIntN type) {
        this(BigInteger.valueOf(value), type);
    }

    public FIntNLiteral(BigInteger value) {
        this.value = value;
    }

    public FIntNLiteral(BigInteger value, FIntN type) {
        this.value = value;
        this.type = type;
    }

    @Override
    public FIntNLiteral copy() {
        return new FIntNLiteral(value, type);
    }

    @Override
    public FClass getType() {
        return type == null ? FIntLiteralType.INSTANCE : type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FIntNLiteral that = (FIntNLiteral) o;

        if (!value.equals(that.value)) return false;
        return type != null ? type.equals(that.type) : that.type == null;
    }

    @Override
    public int hashCode() {
        int result = value.hashCode();
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
