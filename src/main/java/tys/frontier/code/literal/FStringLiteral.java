package tys.frontier.code.literal;

import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FClass;

public class FStringLiteral implements FLiteral { //TODO make sure string literals are immutable

    public static final FArray TYPE = FArray.getArrayFrom(FIntN._8);
    public static final FArray STRING_ARRAY_TYPE = FArray.getArrayFrom(FStringLiteral.TYPE);

    public final String value;

    public FStringLiteral(String value) {
        this.value = value;
    }

    @Override
    public FStringLiteral copy() {
        return this;
    }

    @Override
    public String getOriginalString() {
        return value;
    }

    @Override
    public FClass getType() {
        return TYPE;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FStringLiteral)) return false;

        FStringLiteral that = (FStringLiteral) o;

        return value != null ? value.equals(that.value) : that.value == null;
    }

    @Override
    public int hashCode() {
        return value != null ? value.hashCode() : 0;
    }

    @Override
    public String toString() {
        return '\"' + value + '\"';
    }
}
