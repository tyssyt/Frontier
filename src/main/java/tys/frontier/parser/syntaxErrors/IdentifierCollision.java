package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.identifier.IdentifierNameable;

public class IdentifierCollision extends SyntaxError {

    public final IdentifierNameable a;
    public final IdentifierNameable b;

    public IdentifierCollision(IdentifierNameable a, IdentifierNameable b) {
        super("between" + a + " and " + b);
        this.a = a;
        this.b = b;
    }
}
