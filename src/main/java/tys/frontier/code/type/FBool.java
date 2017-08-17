package tys.frontier.code.type;

import tys.frontier.code.type.capability.LogicCapable;

public class FBool implements FType, LogicCapable {

    public static final FBool INSTANCE = new FBool();

    private FBool () {}

    @Override
    public String toString() {
        return "bool";
    }
}
