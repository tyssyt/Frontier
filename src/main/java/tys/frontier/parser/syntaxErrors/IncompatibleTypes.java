package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.type.FType;

public class IncompatibleTypes extends SyntaxError {

    public final FType expected;
    public final FType actual;

    protected IncompatibleTypes(String message, FType expected, FType actual) {
        super(message);
        this.expected = expected;
        this.actual = actual;
    }

    public IncompatibleTypes(FType expected, FType actual) {
        super("expected: " + expected.getIdentifier() + ", got: " + actual.getIdentifier());
        this.expected = expected;
        this.actual = actual;
    }
}
