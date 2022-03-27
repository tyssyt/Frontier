package tys.frontier.code.literal;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;

public class FNull implements FLiteral {

    private static final FIdentifier IDENTIFIER = new FIdentifier("!NullType");
    //TODO @PositionForGeneratedCode
    public static FClass NULL_TYPE = new FBaseClass(null, IDENTIFIER, FVisibilityModifier.EXPORT, null);
    public static final FNull UNTYPED = new FNull(NULL_TYPE);

    private FType type;

    public FNull(FType type) {
        assert type == NULL_TYPE || FOptional.canBeTreatedAsOptional(type);
        this.type = type;
    }

    @Override
    public FLiteral copy() {
        return UNTYPED;
    }

    @Override
    public FType getType() {
        return type;
    }

    @Override
    public String toString() {
        return "null";
    }

}
