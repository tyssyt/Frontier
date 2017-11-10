package tys.frontier.parser.syntaxErrors;

import java.util.Collection;

public class SyntaxErrors extends Exception {

    public final Collection<? extends SyntaxError> errors;

    public SyntaxErrors(Collection<? extends SyntaxError> errors) {
        assert !errors.isEmpty();
        this.errors = errors;
    }
}
