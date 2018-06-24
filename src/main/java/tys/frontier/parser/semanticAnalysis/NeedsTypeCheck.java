package tys.frontier.parser.semanticAnalysis;

import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public interface NeedsTypeCheck {

    void checkTypes() throws IncompatibleTypes;

    @SuppressWarnings("unchecked")
    default <T> T castArgsTrusted() {
        try {
            checkTypes();
        } catch (IncompatibleTypes incompatibleTypes) {
            Utils.cantHappen();
        }
        return (T) this;
    }

    static void checkAll(Collection<NeedsTypeCheck> checks) throws SyntaxErrors {
        List<IncompatibleTypes> errors = new ArrayList<>();
        for (NeedsTypeCheck c : checks) {
            try {
                c.checkTypes();
            } catch (IncompatibleTypes e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

}
