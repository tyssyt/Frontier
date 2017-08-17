package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;

public class SignatureCollision extends SyntaxError {

    private FFunction a;
    private FFunction b;
    private FClass clazz;

    public SignatureCollision(FFunction a, FFunction b, FClass clazz) {
        super("between " + a + " and " + b + " in class " + clazz.getIdentifier());
        this.a = a;
        this.b = b;
        this.clazz = clazz;
    }
}
