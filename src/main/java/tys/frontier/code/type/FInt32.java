package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArithCapable;

public class FInt32 implements FType, ArithCapable {

    public static final FInt32 INSTANCE = new FInt32();

    private FInt32 () {}

    @Override
    public String toString() {
        return "int32";
    }
}
