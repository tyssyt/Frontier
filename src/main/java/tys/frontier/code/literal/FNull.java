package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FVoid;

public class FNull implements FLiteral {

    public static final FNull INSTANCE = new FNull();

    private FNull () {}

    @Override
    public FClass getType() {
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
