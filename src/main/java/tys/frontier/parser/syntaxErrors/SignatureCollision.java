package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;

public class SignatureCollision extends SyntaxError {

    public final FFunction a;
    public final FFunction b;
    public final FClass clazz;

    public SignatureCollision(FFunction a, FFunction b, FClass clazz) {
        super("between " + a.headerToString() + " and " + b.headerToString() + " in class " + clazz.getIdentifier());
        this.a = a;
        this.b = b;
        this.clazz = clazz;
    }
}
