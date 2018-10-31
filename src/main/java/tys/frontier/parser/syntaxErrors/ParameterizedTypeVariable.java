package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FLocalVariable;

public class ParameterizedTypeVariable extends SyntaxError {

    public final FLocalVariable variable;

    public ParameterizedTypeVariable(FLocalVariable variable) {
        super("Type Variable " + variable + " has parameters");
        this.variable = variable;
    }
}
