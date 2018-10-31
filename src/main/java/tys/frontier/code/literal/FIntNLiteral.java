package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FFloat32;
import tys.frontier.code.predefinedClasses.FFloat64;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.IntLiteralTooLarge;
import tys.frontier.util.Utils;

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
        this.type = FIntN.getIntN(64); //TODO currently this is the largest suppoted type, change once we deal with larger ints
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
    public FLiteral specify(FType targetType) throws IncompatibleTypes {
        if (getType() == targetType)
            return this;
        if (targetType instanceof FIntN) {
            int targetBitWidth = ((FIntN) targetType).getN();
            FIntN newType = FIntN.getIntN(targetBitWidth);
            if (newType.canRepresent(value)) {
                FIntNLiteral res = new FIntNLiteral(value, originalString);
                res.type = newType;
                return res;
            }
            throw new IntLiteralTooLarge(this, newType);
        }
        if (targetType instanceof FFloat32) {
            return new FFloat32Literal(value.floatValue(), originalString);
        }
        if (targetType instanceof FFloat64) {
            return new FFloat64Literal(value.doubleValue(), originalString);
        }
        throw new IncompatibleTypes(targetType, getType());
    }

    @Override
    public int distance(FLiteral other) {
        if (this==other)
            return 0;
        if (other instanceof FIntNLiteral) {
            FIntNLiteral o = ((FIntNLiteral) other);
            if (this.value.equals(o.value))
                return this.type.getN() - o.type.getN();
        }
        return Utils.cantHappen();
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
