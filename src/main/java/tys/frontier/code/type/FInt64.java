package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArithCapable;

public class FInt64 implements FType, ArithCapable {

    public static final FInt64 INSTNACE = new FInt64();

    private FInt64 () {}

    @Override
    public String toString() {
        return "int64";
    }
}
