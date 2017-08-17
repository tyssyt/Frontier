package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArithCapable;

public class FFloat32 implements FType, ArithCapable {

    public static final FFloat32 INSTANCE = new FFloat32();

    private FFloat32 () {}

    @Override
    public String toString() {
        return "float32";
    }
}
