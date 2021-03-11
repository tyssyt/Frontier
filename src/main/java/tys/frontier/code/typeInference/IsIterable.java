package tys.frontier.code.typeInference;

import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.type.FType;

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
