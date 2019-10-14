package tys.frontier.code.type;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
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
import tys.frontier.util.expressionListToTypeListMapping.ExpressionListToTypeListMapping;

import java.util.List;
import java.util.Map;

public class FTypeVariable implements FType {

    public static class ReturnTypeOf extends FTypeVariable {

        private FFunction function;
        private List<FType> positionalArgs;
        private Map<FIdentifier, FType> keywordArgs;

        public ReturnTypeOf(FTypeIdentifier identifier, boolean fixed, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs) {
            super(identifier, fixed, TypeConstraints.create());
            this.positionalArgs = positionalArgs;
            this.keywordArgs = keywordArgs;
        }

        public FFunction getFunction() {
            return function;
        }

        public List<FType> getPositionalArgs() {
            return positionalArgs;
        }

        public Map<FIdentifier, FType> getKeywordArgs() {
            return keywordArgs;
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
    public FunctionResolver.Result softResolveFunction(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType) throws FunctionNotFound {
        if (this.constraints.isResolved())
            return this.constraints.getResolved().softResolveFunction(identifier, positionalArgs, keywordArgs, returnType);

        HasCall constraint = new HasCall(null, identifier, positionalArgs, keywordArgs);
        if (!tryAddConstraint(constraint))
            throw new FunctionNotFound(identifier, positionalArgs, keywordArgs);

        //just return some fitting dummy function
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> paramsBuilder = ImmutableList.builder();
        for (FType arg : positionalArgs) {
            FIdentifier id = arg == FTypeType.INSTANCE ? new FTypeIdentifier(paramNames.next()) : new FVariableIdentifier(paramNames.next());
            paramsBuilder.add(FParameter.create(id, arg, false));
        }
        for (Map.Entry<FIdentifier, FType> entry : keywordArgs.entrySet()) {
            paramsBuilder.add(FParameter.create(entry.getKey(), entry.getValue(), false));
        }
        ImmutableList<FParameter> params = paramsBuilder.build();
        //TODO we might have constraints on the return type, if we are fixed we must have constraints and maybe the return type is fixed as well?
        if (returnType == null)
            returnType = new ReturnTypeOf(new FTypeIdentifier(returnTypeNames.next()), isFixed(), positionalArgs, keywordArgs);
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        FBaseFunction f = new FBaseFunction(identifier, this, FVisibilityModifier.EXPORT, true, returnType, params);
        if (returnType instanceof ReturnTypeOf)
            ((ReturnTypeOf) returnType).function = f;

        FunctionResolver.Result res = new FunctionResolver.Result();
        res.function = f;
        List<FType> paramTypes = Utils.typesFromExpressionList(params);
        res.argMapping = ExpressionListToTypeListMapping.createBasic(paramTypes, positionalArgs.size());
        res.constraints = ImmutableMultimap.of(this, constraint);
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
