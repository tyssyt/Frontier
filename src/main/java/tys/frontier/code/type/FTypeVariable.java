package tys.frontier.code.type;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

public class FTypeVariable implements FType {

    private FIdentifier identifier;
    private TypeConstraints constraints;
    private TypeVariableNamespace namespace;

    public static FTypeVariable create(Location location, FIdentifier identifier, boolean fixed) {
        return new FTypeVariable(location, identifier, fixed);
    }

    protected FTypeVariable(Location location, FIdentifier identifier, boolean fixed) {
        this.identifier = identifier;
        this.constraints = new TypeConstraints(this);
        this.namespace = new TypeVariableNamespace(location, this);
        if (fixed)
            constraints.setFixed();
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

    public FClass getResolved() {
        return constraints.getResolved();
    }

    public FClass hardResolve() throws UnfulfillableConstraints {
        return constraints.hardResolve();
    }

    public boolean tryAddConstraint(ImplicitCastable implicitCastable) {
        if (isFixed()) {
            return constraints.satisfies(implicitCastable);
        } else {
            try {
                constraints.add(implicitCastable);
                return true;
            } catch (UnfulfillableConstraints unfulfillableConstraints) {
                return false;
            }
        }
    }

    public boolean tryAddConstraint(HasCall hasCall) {
        if (isFixed()) {
            return constraints.satisfies(hasCall);
        } else {
            try {
                constraints.add(hasCall);
                return true;
            } catch (UnfulfillableConstraints unfulfillableConstraints) {
                return false;
            }
        }
    }

    @Override
    public Namespace getNamespace() {
        return namespace;
    }

    public FTypeVariable shallowNonFixedCopy() {
        return new FTypeVariable(namespace.getLocation(), identifier, false);
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
