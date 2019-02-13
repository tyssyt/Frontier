package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.List;

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

    public boolean tryAddConstraint(TypeConstraint constraint) {
        if (fixed)
            return constraints.satisfies(constraint);
        else {
            constraints.add(constraint);
            return true;
        }
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        HasCall constraint = new HasCall(null, identifier, arguments, typeInstantiation);
        if (!tryAddConstraint(constraint))
            throw new FunctionNotFound(identifier, Utils.typesFromExpressionList(arguments));

        //just return some fitting dummy function
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FExpression arg : arguments) {
            FIdentifier id;
            if (arg.getType() == FTypeType.INSTANCE) {
                id = new FTypeIdentifier(paramNames.next());
            } else {
                id = new FVariableIdentifier(paramNames.next());
            }
            params.add(FParameter.create(id, arg.getType(), false));
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
