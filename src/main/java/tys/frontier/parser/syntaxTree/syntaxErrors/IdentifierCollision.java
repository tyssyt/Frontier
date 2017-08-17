package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.identifier.IdentifierNameable;

public class IdentifierCollision extends SyntaxError {

    private IdentifierNameable a;
    private IdentifierNameable b;
    private FClass clazz;

    public IdentifierCollision(IdentifierNameable a, IdentifierNameable b, FClass clazz) {
        super("between" + a + " and " + b + " in class" + clazz.getIdentifier());
        this.a = a;
        this.b = b;
        this.clazz = clazz;
    }
}
