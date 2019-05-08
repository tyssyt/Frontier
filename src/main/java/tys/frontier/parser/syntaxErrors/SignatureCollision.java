package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.function.FFunction;

public class SignatureCollision extends SyntaxError {

    public final FFunction a;
    public final FFunction b;

    public SignatureCollision(FFunction a, FFunction b) {
        super("between " + a.headerToString() + " and " + b.headerToString());
        this.a = a;
        this.b = b;
    }
}
