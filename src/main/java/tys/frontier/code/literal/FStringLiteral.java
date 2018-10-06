package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FStringLiteral implements FLiteral { //TODO make sure string literals are immutable

    public static final FArray TYPE = FArray.getArrayFrom(FIntN._8);

    public final String value;

    public FStringLiteral(String value) {
        this.value = value;
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
    public FLiteral specify(FClass targetType) throws IncompatibleTypes {
        if (getType() == targetType)
            return this;
        if (targetType == FIntN._8 && value.length() == 1) {
            return new FCharLiteral(value.charAt(0));
        }
        throw new IncompatibleTypes(targetType, getType());
    }

    @Override
    public int distance(FLiteral other) {
        if (this==other)
            return 0;
        if (other instanceof FCharLiteral) {
            FCharLiteral o = ((FCharLiteral) other);
            if (this.getOriginalString().equals(o.getOriginalString()))
                return 128;
        }
        return Utils.cantHappen();
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
