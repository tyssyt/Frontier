package tys.frontier.code.literal;

import com.koloboke.collect.map.hash.HashCharCharMap;
import com.koloboke.collect.map.hash.HashCharCharMaps;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FClass;

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
    public FCharLiteral copy() {
        return this;
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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FCharLiteral)) return false;

        FCharLiteral that = (FCharLiteral) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return value;
    }

    @Override
    public String toString() {
        return "" + '\'' + value + '\'';
    }
}
