package tys.frontier.code;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FTypeVariable extends FType {

    FLocalVariable variable; //TODO this actually should not be LOCAL variables, but it works...

    public FTypeVariable(FLocalVariable variable) {
        super((FTypeIdentifier) variable.getIdentifier());
        this.variable = variable;
    }

    public FLocalVariable getVariable() {
        return variable;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(identifier.name);
    }
}
