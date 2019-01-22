package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.Utils;

import java.util.List;

public class FTypeVariable implements FType {

    private FVariable variable; //TODO why does no one need access to this?

    public FTypeVariable(FVariable variable) {
        assert variable.getType() instanceof FTypeType;
        this.variable = variable;
    }

    @Override
    public FTypeIdentifier getIdentifier() {
        return (FTypeIdentifier) variable.getIdentifier();
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        return Utils.NYI("function calls on Type Variables");
    }

    public FTypeVariable copy() {
        return new FTypeVariable(variable);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }

    @Override
    public String toString() {
        return tS();
    }
}
