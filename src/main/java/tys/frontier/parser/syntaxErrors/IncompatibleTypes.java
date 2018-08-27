package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FType;

public class IncompatibleTypes extends SyntaxError {

    public final FType expected;
    public final FType actual;

    public IncompatibleTypes(FType expected, FType actual) {
        super("expected: " + expected.getIdentifier() + ", got: " + actual.getIdentifier());
        this.expected = expected;
        this.actual = actual;
    }
}
