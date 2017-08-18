package tys.frontier.code.statement;

import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

public interface NeedsTypeCheck {

    void checkTypes() throws IncompatibleTypes;

}
