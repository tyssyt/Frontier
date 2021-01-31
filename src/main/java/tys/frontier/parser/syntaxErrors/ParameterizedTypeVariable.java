package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.type.FTypeVariable;

public class ParameterizedTypeVariable extends SyntaxError {

    public final FTypeVariable variable;

    public ParameterizedTypeVariable(FTypeVariable variable) {
        super(variable.getNamespace().getLocation().getPoint(), "Type Variable " + variable + " has parameters");
        this.variable = variable;
    }
}
