package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FBool;

public class FBoolLiteral implements FLiteral {

    public static final FBoolLiteral TRUE = new FBoolLiteral(true);
    public static final FBoolLiteral FALSE = new FBoolLiteral(false);

    public final boolean value;

    private FBoolLiteral(boolean value) {
        this.value = value;
    }

    @Override
    public FBoolLiteral copy() {
        return this;
    }

    @Override
    public FClass getType() {
        return FBool.INSTANCE;
    }

    @Override
    public String toString() {
        return "" + value;
    }
    @Override
    public String getOriginalString() {
        return toString();
    }
}
