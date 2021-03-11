package tys.frontier.code.typeInference;

public abstract class TypeConstraint {

    private Object origin; //TODO an interface that groups all possible origins

    public TypeConstraint(Object origin) {
        this.origin = origin;
    }

    public Object getOrigin() {
        return origin;
    }

    public void setOrigin(Object origin) {
        this.origin = origin;
    }

    @Override
    abstract public int hashCode();
    @Override
    abstract public boolean equals(Object obj);


}
