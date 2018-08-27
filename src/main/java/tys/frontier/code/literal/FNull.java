package tys.frontier.code.literal;

import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FVoid;

public class FNull implements FLiteral {

    public static final FNull INSTANCE = new FNull();

    private FNull () {}

    @Override
    public FType getType() {
        return FVoid.INSTANCE;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public String getOriginalString() {
        return toString();
    }
}
