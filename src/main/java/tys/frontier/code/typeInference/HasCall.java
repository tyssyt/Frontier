package tys.frontier.code.typeInference;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.List;

public class HasCall extends TypeConstraint {

    private FFunctionIdentifier identifier;
    private List<FExpression> arguments;
    private TypeInstantiation typeInstantiation;

    public HasCall(FExpression origin, FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) {
        super(origin);
        this.identifier = identifier;
        this.arguments = arguments;
        this.typeInstantiation = typeInstantiation;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof HasCall)) return false;

        HasCall hasCall = (HasCall) o;

        if (!identifier.equals(hasCall.identifier)) return false;
        if (!arguments.equals(hasCall.arguments)) return false;
        return typeInstantiation.equals(hasCall.typeInstantiation);
    }

    @Override
    public int hashCode() {
        int result = identifier.hashCode();
        result = 31 * result + arguments.hashCode();
        result = 31 * result + typeInstantiation.hashCode();
        return result;
    }
}
