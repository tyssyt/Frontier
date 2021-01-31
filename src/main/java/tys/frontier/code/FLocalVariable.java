package tys.frontier.code;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;

public class FLocalVariable extends FVariable {

    private Position position;

    public FLocalVariable(Position position, FIdentifier identifier, FType type) {
        super(identifier, type);
        this.position = position;
    }

    public Position getPosition() {
        return position;
    }
}
