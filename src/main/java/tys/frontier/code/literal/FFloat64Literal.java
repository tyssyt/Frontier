package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FFloat64;

public class FFloat64Literal implements FLiteral {

    public final double value;
    public final String originalString;

    public FFloat64Literal(double value, String originalString) {
        this.value = value;
        this.originalString = originalString;
    }

    @Override
    public FFloat64Literal copy() {
        return this;
    }

    @Override
    public FClass getType() {
        return FFloat64.INSTANCE;
    }

    @Override
    public String getOriginalString() {
        return originalString;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FFloat64Literal that = (FFloat64Literal) o;

        return Double.compare(that.value, value) == 0;
    }

    @Override
    public int hashCode() {
        long temp = Double.doubleToLongBits(value);
        return (int) (temp ^ (temp >>> 32));
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
