package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FClass;

public class IncompatibleTypes extends SyntaxError {

    public final FClass expected;
    public final FClass actual;

    public IncompatibleTypes(FClass expected, FClass actual) {
        super("expected: " + expected + ", got: " + actual);
        this.expected = expected;
        this.actual = actual;
    }
}
