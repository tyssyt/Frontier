package tys.frontier.code.literal;

import it.unimi.dsi.fastutil.chars.Char2CharMap;
import it.unimi.dsi.fastutil.chars.Char2CharMaps;
import it.unimi.dsi.fastutil.chars.Char2CharOpenHashMap;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.type.FClass;

public class FCharLiteral implements FLiteral {

    public static final Char2CharMap escapeLiterals = Char2CharMaps.unmodifiable(new Char2CharOpenHashMap(
            new char[] {'n', '0', '\\','\''},
            new char[] {'\n', '\0', '\\','\''}
    ));

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
