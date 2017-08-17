package tys.frontier.code.type;

import tys.frontier.code.type.capability.ArithCapable;

public class FFloat64 implements FType, ArithCapable {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {}

    @Override
    public String toString() {
        return "float64";
    }
}
