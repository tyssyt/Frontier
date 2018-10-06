package tys.frontier.code.literal;

import com.koloboke.collect.map.hash.HashCharCharMap;
import com.koloboke.collect.map.hash.HashCharCharMaps;
import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.util.Utils;

public class FCharLiteral implements FLiteral {

    public static final HashCharCharMap escapeLiterals = HashCharCharMaps.newImmutableMap(
            new char[] {'n' ,'\\','\''},
            new char[] {'\n','\\','\''}
    );

    public final char value;

    public FCharLiteral(char value) {
        this.value = value;
    }

    @Override
    public String getOriginalString() {
        return value+"";
    }

    @Override
    public FClass getType() {
        return FIntN._8;
    }

    @Override
    public int distance(FLiteral other) {
        if (this==other)
            return 0;
        if (other instanceof FStringLiteral) {
            FStringLiteral o = ((FStringLiteral) other);
            if (this.getOriginalString().equals(o.getOriginalString()))
                return -128;
        }
        return Utils.cantHappen();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FCharLiteral)) return false;

        FCharLiteral that = (FCharLiteral) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return (int) value;
    }

    @Override
    public String toString() {
        return "" + '\'' + value + '\'';
    }
}
