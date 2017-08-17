package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FInt64;

public class FInteger64Literal implements FLiteral {

    public final long value;

    public FInteger64Literal(long value) {
        this.value = value;
    }

    @Override
    public FClass getType() {
        return FInt64.INSTANCE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FInteger64Literal that = (FInteger64Literal) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return (int) (value ^ (value >>> 32));
    }
}
