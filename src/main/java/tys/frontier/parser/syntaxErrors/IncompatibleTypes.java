package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;

public class IncompatibleTypes extends SyntaxError {

    public final FType expected;
    public final FType actual;

    protected IncompatibleTypes(Position position, String message, FType expected, FType actual) {
        super(position, message);
        this.expected = expected;
        this.actual = actual;
    }

    public IncompatibleTypes(FType expected, FType actual) {
        super("expected: " + expected.getIdentifier() + ", got: " + actual.getIdentifier());
        this.expected = expected;
        this.actual = actual;
    }

    public IncompatibleTypes(Position position, FType expected, FType actual) {
        super(position, "expected: " + expected.getIdentifier() + ", got: " + actual.getIdentifier());
        this.expected = expected;
        this.actual = actual;
    }
}
