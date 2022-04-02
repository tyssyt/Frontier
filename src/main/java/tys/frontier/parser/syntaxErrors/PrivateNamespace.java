package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.location.Position;

public class PrivateNamespace extends SyntaxError {

    public PrivateNamespace(Position position, FIdentifier identifier) {
        super(position, "Namespace/class may not be private: " + identifier);
    }
}
