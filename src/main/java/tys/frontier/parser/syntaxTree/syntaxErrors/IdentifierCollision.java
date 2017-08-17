package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FIdentifierNameable;

public class IdentifierCollision extends SyntaxError {

    private FIdentifierNameable a;
    private FIdentifierNameable b;
    private FClass clazz;

    public IdentifierCollision(FIdentifierNameable a, FIdentifierNameable b, FClass clazz) {
        super("between" + a + " and " + b + " in class" + clazz.getIdentifier());
        this.a = a;
        this.b = b;
        this.clazz = clazz;
    }
}
