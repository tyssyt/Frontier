package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FInt;

import java.math.BigInteger;

public class FIntegerLiteral implements FLiteral {

    public final BigInteger value;


    public FIntegerLiteral(BigInteger value) {
        this.value = value;
    }

    public FIntegerLiteral(long value) {
        this.value = BigInteger.valueOf(value);
    }

    @Override
    public FClass getType() {
        return FInt.INSTANCE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FIntegerLiteral that = (FIntegerLiteral) o;

        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }
}
