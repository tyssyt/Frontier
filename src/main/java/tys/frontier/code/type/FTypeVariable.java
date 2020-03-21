package tys.frontier.code.type;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.statement.loop.forImpl.FTypeVariableForImpl;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

public class FTypeVariable implements FType {

    private FIdentifier identifier;
    private TypeConstraints constraints;
    private FTypeVariableForImpl forImpl = new FTypeVariableForImpl(this);
    private TypeVariableNamespace namespace;

    public static FTypeVariable create(FIdentifier identifier, boolean fixed) {
        return new FTypeVariable(identifier, fixed, TypeConstraints.create());
    }

    protected FTypeVariable(FIdentifier identifier, boolean fixed, TypeConstraints constraints) {
        this.identifier = identifier;
        this.constraints = constraints;
        this.namespace = new TypeVariableNamespace(this);
        if (fixed)
            constraints.setFixed();
        constraints.addVar(this);
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public long concreteness() {
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

    public void setConstraints(TypeConstraints constraints) {
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
            if (constraints.satisfies(constraint))
                return true;
            //special case: cast to a nonFixed constraint, force it to be equal to us
            if (constraint instanceof ImplicitCastable && ((ImplicitCastable) constraint).getTarget() instanceof FTypeVariable) {
                FTypeVariable target = (FTypeVariable) ((ImplicitCastable) constraint).getTarget();
                if (!target.isFixed())
                    return target.tryAddConstraint(new ImplicitCastable(constraint, this, Variance.Invariant));
            }
            return false;
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

    @Override
    public ForImpl getForImpl() {
        if (isResolved())
            return getResolved().getForImpl();
        return forImpl;
    }

    public FTypeVariable copy() {
        return new FTypeVariable(identifier, isFixed(), constraints.copy());
    }

    public FTypeVariable copy(boolean fixed) {
        return new FTypeVariable(identifier, fixed, constraints.copy());
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
