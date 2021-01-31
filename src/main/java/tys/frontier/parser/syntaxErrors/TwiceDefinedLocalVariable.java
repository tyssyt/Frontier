package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.location.Position;

public class TwiceDefinedLocalVariable extends SyntaxError {

    public final FIdentifier identifier;

    public TwiceDefinedLocalVariable(Position position, Position position2, FIdentifier identifier) {
        super(position, position2, identifier.toString());
        this.identifier = identifier;
    }
}
