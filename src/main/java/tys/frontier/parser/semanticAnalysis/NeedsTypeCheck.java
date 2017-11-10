package tys.frontier.parser.semanticAnalysis;

import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public interface NeedsTypeCheck {

    void checkTypes() throws IncompatibleTypes;

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
            throw new SyntaxErrors(errors);
    }

}
