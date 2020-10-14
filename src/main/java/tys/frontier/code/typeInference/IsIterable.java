package tys.frontier.code.typeInference;

import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Utils;

public class IsIterable extends TypeConstraint implements ForImpl {

    private FType elementType;

    public IsIterable(Object origin, FType elementType) {
        super(origin);
        this.elementType = elementType;
    }

    @Override
    public FType getElementType() {
        return elementType;
    }

    @Override
    public TypeConstraint copy() {
        if (elementType instanceof TypeVariableNamespace.IterationElementType)
            return new IsIterable(this, ((TypeVariableNamespace.IterationElementType) elementType).copy());
        else if (elementType instanceof FTypeVariable)
            return Utils.NYI("TypeVariable DeepCopy"); //TODO see TypeConstraint
        else
            return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IsIterable isIterable = (IsIterable) o;

        return elementType.equals(isIterable.elementType);
    }

    @Override
    public int hashCode() {
        return elementType.hashCode();
    }

    @Override
    public String toString() {
        return "iterable: " + elementType.getIdentifier().name;
    }
}
