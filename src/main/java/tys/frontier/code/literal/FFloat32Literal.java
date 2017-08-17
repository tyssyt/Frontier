package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FFloat32;

public class FFloat32Literal implements FLiteral {

    public final float value;

    public FFloat32Literal(float value) {
        this.value = value;
    }

    @Override
    public FClass getType() {
        return FFloat32.INSTANCE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        FFloat32Literal that = (FFloat32Literal) o;

        return Float.compare(that.value, value) == 0;
    }

    @Override
    public int hashCode() {
        return (value != +0.0f ? Float.floatToIntBits(value) : 0);
    }
}
