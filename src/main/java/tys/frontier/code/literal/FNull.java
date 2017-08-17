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
}
