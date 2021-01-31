package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.location.Position;

public class UndeclaredVariable extends SyntaxError {

    public final FIdentifier identifier;

    public UndeclaredVariable(Position position, FIdentifier identifier) {
        super(position, identifier.toString());
        this.identifier = identifier;
    }
}
