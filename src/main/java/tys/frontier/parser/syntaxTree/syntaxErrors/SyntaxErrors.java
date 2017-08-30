package tys.frontier.parser.syntaxTree.syntaxErrors;

import java.util.Collection;

public class SyntaxErrors extends Exception {

    public final Collection<? extends SyntaxError> errors;

    public SyntaxErrors(Collection<? extends SyntaxError> errors) {
        this.errors = errors;
    }
}
