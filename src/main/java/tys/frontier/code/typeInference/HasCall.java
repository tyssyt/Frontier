package tys.frontier.code.typeInference;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.type.FType;

import java.util.List;

public class HasCall extends TypeConstraint {

    //TODO can there be a case where we know the return Type and want a member for that?
    private FFunctionIdentifier identifier;
    private List<FType> argumentTypes;
    private TypeInstantiation typeInstantiation;

    public HasCall(FExpression origin, FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation) {
        super(origin);
        this.identifier = identifier;
        this.argumentTypes = argumentTypes;
        this.typeInstantiation = typeInstantiation;
    }

    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    public List<FType> getArgumentTypes() {
        return argumentTypes;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
    }

    @Override
    public String toString() {
        return "HasCall{" + identifier + '(' + argumentTypes + "), " + typeInstantiation + '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof HasCall)) return false;

        HasCall hasCall = (HasCall) o;

        if (!identifier.equals(hasCall.identifier)) return false;
        if (!argumentTypes.equals(hasCall.argumentTypes)) return false;
        return typeInstantiation.equals(hasCall.typeInstantiation);
    }

    @Override
    public int hashCode() {
        int result = identifier.hashCode();
        result = 31 * result + argumentTypes.hashCode();
        result = 31 * result + typeInstantiation.hashCode();
        return result;
    }
}
