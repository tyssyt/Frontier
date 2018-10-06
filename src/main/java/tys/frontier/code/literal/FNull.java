package tys.frontier.code.literal;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.predefinedClasses.FVoid;

public class FNull implements FLiteral {

    public static final FNull UNTYPED = new FNull(null);

    private FOptional type;

    public FNull(FOptional type) {
        this.type = type;
    }

    @Override
    public FClass getType() {
        return type == null ? FVoid.INSTANCE : type;
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
