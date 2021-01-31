package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

public class TupleFor implements ForImpl {

    private FType elementType = FTypeVariable.create(null, new FIdentifier("!ElementTypeTupleFor"), false);

    @Override
    public FType getElementType() {
        return elementType;
    }
}
