package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class IncompatibleTypes extends SyntaxError {

    public final FClass expected;
    public final FClass actual;

    public IncompatibleTypes(FClass expected, FClass actual) {
        super("expected: " + expected.getIdentifier() + ", got: " + actual.getIdentifier());
        this.expected = expected;
        this.actual = actual;
    }
}
