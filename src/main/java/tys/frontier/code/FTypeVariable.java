package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import tys.frontier.code.expression.FFunctionCall;
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
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Map;

public class FTypeVariable implements FType {

    private FTypeIdentifier identifier;
    private boolean fixed;
    private TypeConstraints constraints;
    private NameGenerator returnTypeNames;

    public static FTypeVariable create(FTypeIdentifier identifier, boolean fixed) {
        TypeConstraints constraints = fixed ? TypeConstraints.EMPTY : TypeConstraints.create();
        return new FTypeVariable(identifier, fixed, constraints);
    }

    protected FTypeVariable(FTypeIdentifier identifier, boolean fixed, TypeConstraints constraints) {
        this.identifier = identifier;
        this.fixed = fixed;
        this.constraints = constraints;
        this.returnTypeNames = new NameGenerator("?" + identifier.name + "ret.", "");
    }

    @Override
    public FTypeIdentifier getIdentifier() {
        return identifier;
    }

    public boolean isFixed() {
        return fixed;
    }

    public void setConstraints(TypeConstraints constraints) {
        assert constraints.isEmpty();
        this.constraints = constraints;
    }

    public TypeConstraints getConstraints() {
        return constraints;
    }

    public boolean tryAddConstraint(TypeConstraint constraint) {
        if (fixed)
            return constraints.satisfies(constraint);
        else {
            constraints.add(constraint);
            return true;
        }
    }

    @Override
    public FField getField(FIdentifier identifier) throws FieldNotFound {
        return Utils.NYI(""); //TODO
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        ArrayListMultimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
        FFunction res = resolveFunction(identifier, argumentTypes, typeInstantiation, constraints);
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : constraints.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new FunctionNotFound(identifier, argumentTypes);
        }
        return res;
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound {
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
        FTypeVariable returnType = create(new FTypeIdentifier(returnTypeNames.next()), fixed);
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        return new FFunction(identifier, this, FVisibilityModifier.EXPORT, true, returnType, params.build()) {
            @Override
            public boolean addCall(FFunctionCall call) { //this is a "hack" to set the origin of the constraint
                constraint.setOrigin(call);
                return super.addCall(call);
            }
        };
    }

    public FTypeVariable copy() {
        return new FTypeVariable(identifier, fixed, constraints.copy());
    }

    public FTypeVariable copy(boolean fixed) {
        return new FTypeVariable(identifier, fixed, constraints.copy());
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getIdentifier().name);
        if (!fixed)
            sb.append('*');
        return sb;
    }

    @Override
    public String toString() {
        return tS();
    }
}
