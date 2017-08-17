package tys.frontier.code.literal;

import tys.frontier.code.type.FType;
import tys.frontier.code.type.FVoid;

public class FNull implements FLiteral {

    public static final FNull INSTANCE = new FNull();

    private FNull () {}

    @Override
    public FType getType() {
        return FVoid.INSTANCE;
    }
}
