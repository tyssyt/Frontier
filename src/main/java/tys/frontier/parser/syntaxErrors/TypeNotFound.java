package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.location.Position;

public class TypeNotFound extends SyntaxError {

    public final FIdentifier identifier;

    public TypeNotFound(Position position, FIdentifier identifier) {
        super(position, identifier.toString());
        this.identifier = identifier;
    }
}
