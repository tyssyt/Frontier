package tys.frontier.code.statement.loop.forImpl;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FTypeVariable;

public class TupleFor implements ForImpl {

    private FTypeVariable elementType = FTypeVariable.create(null, new FIdentifier("!ElementTypeTupleFor"), false);

    @Override
    public FTypeVariable getElementType() {
        return elementType;
    }
}
