package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FFieldType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

public class PrimitiveFor implements ForImpl {

    private FType elementType = FTuple.from(
            FTypeVariable.create(null, new FIdentifier("!ElementTypePrimitiveFor"), true),
            FFieldType.INSTANCE);

    @Override
    public FType getElementType() {
        return elementType;
    }
}
