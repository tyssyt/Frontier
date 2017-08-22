package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FInt32;

public class FInt32Literal implements FLiteral {

    public final int value;

    public FInt32Literal(int value) {
        this.value = value;
    }

    @Override
    public FClass getType() {
        return FInt32.INSTANCE;
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
}
