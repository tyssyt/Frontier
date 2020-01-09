package tys.frontier.code.type;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimaps;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.statement.loop.forImpl.FTypeVariableForImpl;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;

public class FTypeVariable implements FType {

    public static class ReturnTypeOf extends FTypeVariable {

        private FFunction function;
        private List<FType> positionalArgs;
        private ListMultimap<FIdentifier, FType> keywordArgs;
        private boolean lhsResolve;

        public ReturnTypeOf(FTypeIdentifier identifier, boolean fixed, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, boolean lhsResolve) {
            super(identifier, fixed, TypeConstraints.create());
            this.positionalArgs = positionalArgs;
            this.keywordArgs = keywordArgs;
            this.lhsResolve = lhsResolve;
        }

        public FTypeVariable getBase() {
            return (FTypeVariable) function.getMemberOf();
        }

        public FFunction getFunction() {
            return function;
        }

        public List<FType> getPositionalArgs() {
            return positionalArgs;
        }

        public ListMultimap<FIdentifier, FType> getKeywordArgs() {
            return keywordArgs;
        }

        public boolean isLhsResolve() {
            return lhsResolve;
        }
    }

    private FTypeIdentifier identifier;
    private TypeConstraints constraints;
    private NameGenerator returnTypeNames;
    private FTypeVariableForImpl forImpl = new FTypeVariableForImpl(this);

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
        if (isFixed())
            return constraints.satisfies(constraint);
        else {
            try {
                constraints = TypeConstraints.add(constraints, constraint);
            } catch (UnfulfillableConstraints unfulfillableConstraints) {
                return false;
            }
            return true;
        }
    }

    @Override
    public ForImpl getForImpl() {
        if (isResolved())
            return getResolved().getForImpl();
        return forImpl;
    }

    @Override
    public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        if (this.isResolved())
            return this.getResolved().softResolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve);

        /* TODO
            on LHS resolve, I can assume returnType VOID, but I need to add another parameter of unknown type (placeholder for assignees)
            and use the same ReturnTypeOf Mechanism I have for that parameter
            At least thats my current best guess...
         */
        if (lhsResolve)
            return Utils.NYI("Lhs resolve on TypeVariable");

        HasCall constraint = new HasCall(null, identifier, positionalArgs, keywordArgs, lhsResolve);
        if (!tryAddConstraint(constraint))
            throw new FunctionNotFound(identifier, positionalArgs, keywordArgs);

        //just return some fitting dummy function
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> paramsBuilder = ImmutableList.builder();
        for (FType arg : positionalArgs) {
            FIdentifier id = arg == FTypeType.INSTANCE ? new FTypeIdentifier(paramNames.next()) : new AttributeIdentifier(paramNames.next());
            paramsBuilder.add(FParameter.create(id, arg, false));
        }
        for (Map.Entry<FIdentifier, List<FType>> entry : Multimaps.asMap(keywordArgs).entrySet()) {
            paramsBuilder.add(FParameter.create(entry.getKey(), FTuple.from(entry.getValue()), false));
        }
        ImmutableList<FParameter> params = paramsBuilder.build();
        //TODO we might have constraints on the return type, if we are fixed we must have constraints and maybe the return type is fixed as well?
        if (returnType == null)
            returnType = new ReturnTypeOf(new FTypeIdentifier(returnTypeNames.next()), isFixed(), positionalArgs, keywordArgs, lhsResolve);
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        FBaseFunction f = new FBaseFunction(identifier, this, FVisibilityModifier.EXPORT, true, returnType, params, null, emptyMap());
        if (returnType instanceof ReturnTypeOf)
            ((ReturnTypeOf) returnType).function = f;

        FunctionResolver.Result res = new FunctionResolver.Result();
        res.signature = lhsResolve ? f.getLhsSignature() : f.getSignature();
        List<FType> paramTypes = Utils.typesFromExpressionList(params);
        res.argMapping = ArgMapping.createBasic(paramTypes, positionalArgs.size());
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
