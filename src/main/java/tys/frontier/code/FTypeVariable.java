package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.Utils;

import java.util.List;

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

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        return Utils.NYI("function calls on Type Variables");
    }
}
