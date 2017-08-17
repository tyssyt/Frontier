package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.identifier.IdentifierNameable;

public class IdentifierCollision extends SyntaxError {

    public final IdentifierNameable a;
    public final IdentifierNameable b;
    public final FClass clazz;

    public IdentifierCollision(IdentifierNameable a, IdentifierNameable b, FClass clazz) {
        super("between" + a + " and " + b + " in class" + clazz.getIdentifier());
        this.a = a;
        this.b = b;
        this.clazz = clazz;
    }
}
