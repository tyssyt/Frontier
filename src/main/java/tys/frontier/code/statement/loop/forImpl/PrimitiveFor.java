package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FFieldType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

public class PrimitiveFor implements ForImpl {

    public static final FIdentifier IDENTIFIER_ELEMENT_PRIMITIVE_FOR = new FIdentifier("!ElementTypePrimitiveFor");

    private FType elementType = FTuple.from(FTypeVariable.create(null, IDENTIFIER_ELEMENT_PRIMITIVE_FOR), FFieldType.INSTANCE);

    @Override
    public FType getElementType() {
        return elementType;
    }
}
