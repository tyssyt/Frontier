package tys.frontier.code.literal;

import tys.frontier.code.type.FType;

public class FBoolLiteral implements FLiteral {

    public static final FBoolLiteral TRUE = new FBoolLiteral(true);
    public static final FBoolLiteral FALSE = new FBoolLiteral(false);

    public final boolean value;

    private FBoolLiteral(boolean value) {
        this.value = value;
    }

    @Override
    public FType getType() {
        return null;
    }
}
