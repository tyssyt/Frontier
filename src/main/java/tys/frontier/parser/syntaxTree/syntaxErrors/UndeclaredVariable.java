package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FFunction;
import tys.frontier.code.identifier.FVariableIdentifier;

public class UndeclaredVariable extends SyntaxError {

    public final FVariableIdentifier identifier;
    public final FFunction function;

    public UndeclaredVariable(FVariableIdentifier identifier, FFunction function) {
        super(identifier + " in " + function);
        this.identifier = identifier;
        this.function = function;
    }
}
