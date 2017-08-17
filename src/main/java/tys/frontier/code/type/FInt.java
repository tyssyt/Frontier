package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArithCapable;

public class FInt implements FType, ArithCapable {

    public static final FInt INSTANCE = new FInt();

    private FInt () {}

    @Override
    public String toString() {
        return "int";
    }
}
