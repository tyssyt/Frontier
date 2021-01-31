package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.location.Position;

public class UntypedVariable extends SyntaxError {

    public final FIdentifier identifier;

    public UntypedVariable(Position position, FIdentifier identifier) {
        super(position, "untyped Variable: " + identifier);
        this.identifier = identifier;
    }
}
