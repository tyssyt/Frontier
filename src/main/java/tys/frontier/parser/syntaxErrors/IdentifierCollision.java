package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FField;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.namespace.Namespace;

public class IdentifierCollision extends SyntaxError {

    public final IdentifierNameable a;
    public final IdentifierNameable b;

    public IdentifierCollision(FField a, FField b) {
        super(a.getLocation().getPoint(), b.getLocation().getPoint(), "between " + a.getIdentifier() + " and " + b.getIdentifier());
        this.a = a;
        this.b = b;
    }

    public IdentifierCollision(Namespace a, Namespace b) {
        super(a.getLocation().getPoint(), b.getLocation().getPoint(), "between " + a.getIdentifier() + " and " + b.getIdentifier());
        this.a = a;
        this.b = b;
    }
}
