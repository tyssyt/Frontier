package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.List;

public class FTypeVariable implements FType {

    public static class ReturnTypeOf extends FTypeVariable {

        private FFunction function;

        public ReturnTypeOf(FTypeIdentifier identifier, boolean fixed) {
            super(identifier, fixed, TypeConstraints.create());
        }

        public FFunction getFunction() {
            return function;
        }
    }

    private FTypeIdentifier identifier;
    private TypeConstraints constraints;
    private NameGenerator returnTypeNames;

    public static FTypeVariable create(FTypeIdentifier identifier, boolean fixed) {
        return new FTypeVariable(identifier, fixed, TypeConstraints.create());
    }

    protected FTypeVariable(FTypeIdentifier identifier, boolean fixed, TypeConstraints constraints) {
        this.identifier = identifier;
        this.constraints = constraints;
        if (fixed)
            constraints.setFixed();
        constraints.addVar(this);
        this.returnTypeNames = new NameGenerator("?" + identifier.name + "ret.", "");
    }

    @Override
    public FTypeIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public long concreteness() {
        return 0;
    }

    @Override
    public boolean canImplicitlyCast() {
        if (constraints.isResolved())
            return constraints.getResolved().canImplicitlyCast();
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

    public boolean tryAddConstraint(TypeConstraint constraint) {
        if (isFixed())
            return constraints.satisfies(constraint);
        else {
            try {
                TypeConstraints.add(constraints, constraint);
            } catch (UnfulfillableConstraints unfulfillableConstraints) {
                return false;
            }
            return true;
        }
    }

    @Override
    public FField getField(FIdentifier identifier) throws FieldNotFound {
        if (this.constraints.isResolved())
            return this.constraints.getResolved().getField(identifier);
        return Utils.NYI(""); //TODO
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, FType returnType, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound {
        if (this.constraints.isResolved())
            return this.constraints.getResolved().resolveFunction(identifier, argumentTypes, returnType, typeInstantiation, constraints);

        HasCall constraint = new HasCall(null, identifier, argumentTypes, typeInstantiation);
        constraints.put(this, constraint);
        if (!tryAddConstraint(constraint))
            throw new FunctionNotFound(identifier, argumentTypes);

        //just return some fitting dummy function
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FType arg : argumentTypes) {
            FIdentifier id;
            if (arg == FTypeType.INSTANCE) {
                id = new FTypeIdentifier(paramNames.next());
            } else {
                id = new FVariableIdentifier(paramNames.next());
            }
            params.add(FParameter.create(id, arg, false));
        }
        //TODO we might have constraints on the return type, if we are fixed we must have constraints and maybe the return type is fixed as well?
        if (returnType == null)
            returnType = new ReturnTypeOf(new FTypeIdentifier(returnTypeNames.next()), isFixed());
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        FBaseFunction res = new FBaseFunction(identifier, this, FVisibilityModifier.EXPORT, true, returnType, params.build()) {
            @Override
            public boolean addCall(FFunctionCall call) { //this is a "hack" to set the origin of the constraint
                constraint.setOrigin(call);
                return super.addCall(call);
            }
        };
        if (returnType instanceof ReturnTypeOf)
            ((ReturnTypeOf) returnType).function = res;
        return res;
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
