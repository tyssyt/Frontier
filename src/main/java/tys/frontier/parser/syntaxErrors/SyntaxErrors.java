package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.Iterables;

import java.util.Collection;
import java.util.Collections;

public class SyntaxErrors extends Exception {

    public final Collection<? extends SyntaxError> errors;

    private SyntaxErrors(Collection<? extends SyntaxError> errors, String message) {
        super(message, errors.iterator().next());
        this.errors = errors;
    }

    private SyntaxErrors(SyntaxError e) {
        super(e);
        errors = Collections.singleton(e);
    }

    public static SyntaxErrors create(Collection<? extends SyntaxError> errors) {
        assert !errors.isEmpty();
        if (errors.size() == 1)
            return new SyntaxErrors(Iterables.getOnlyElement(errors));
        StringBuilder sb = new StringBuilder("SyntaxErrors:");
        for (SyntaxError e : errors) {
            sb.append('\n').append(e);
        }
        return new SyntaxErrors(errors, sb.toString());
    }
}
