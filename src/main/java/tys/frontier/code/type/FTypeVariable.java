package tys.frontier.code.type;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

public class FTypeVariable implements FType {

    private FIdentifier identifier;
    private TypeConstraints constraints;
    private TypeVariableNamespace namespace;

    public static FTypeVariable create(Location location, FIdentifier identifier, boolean fixed) {
        return new FTypeVariable(location, identifier, fixed, TypeConstraints.create());
    }

    protected FTypeVariable(Location location, FIdentifier identifier, boolean fixed, TypeConstraints constraints) {
        this.identifier = identifier;
        this.constraints = constraints;
        this.namespace = new TypeVariableNamespace(location, this);
        if (fixed)
            constraints.setFixed();
        constraints.addVar(this);
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public int concreteness() {
        return 0;
    }

    @Override
    public boolean canImplicitlyCast() {
        if (isResolved())
            return getResolved().canImplicitlyCast();
        return true; //TODO when fixed we could check the constraints and find cases where we can return false
    }

    public boolean isFixed() {
        return constraints.isFixed();
    }

    @Override
    public ForImpl getForImpl() {
        return constraints.getForImpl();
    }

    public void setConstraints(TypeConstraints constraints) {
        assert constraints.getEquivalenceGroup().contains(this);
        assert constraints.isFixed() || !this.constraints.isFixed();
        this.constraints = constraints;
    }

    public TypeConstraints getConstraints() {
        return constraints;
    }

    public boolean isResolved() {
        return constraints.isResolved();
    }

    public FType getResolved() {
        return constraints.getResolved();
    }

    public FClass hardResolve() throws UnfulfillableConstraints {
        return constraints.hardResolve();
    }

    public boolean tryAddConstraint(TypeConstraint constraint) {
        if (isFixed()) {
            return constraints.satisfies(constraint);
        } else {
            try {
                constraints = TypeConstraints.add(constraints, constraint);
            } catch (UnfulfillableConstraints unfulfillableConstraints) {
                return false;
            }
            return true;
        }
    }

    @Override
    public Namespace getNamespace() {
        return namespace;
    }

    public FTypeVariable copy() {
        return new FTypeVariable(namespace.getLocation(), identifier, isFixed(), constraints.copy());
    }

    public FTypeVariable copy(boolean fixed) {
        return new FTypeVariable(namespace.getLocation(), identifier, fixed, constraints.copy());
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getIdentifier().name);
        if (!isFixed())
            sb.append('*');
        return sb;
    }

    @Override
    public String toString() {
        return tS();
    }
}
