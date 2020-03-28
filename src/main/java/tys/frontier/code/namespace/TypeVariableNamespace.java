package tys.frontier.code.namespace;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimaps;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.DummyFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.HasSelfCall;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;

public class TypeVariableNamespace implements Namespace {

    private FTypeVariable typeVariable;
    private NameGenerator returnTypeNames;

    public TypeVariableNamespace(FTypeVariable typeVariable) {
        this.typeVariable = typeVariable;
        this.returnTypeNames = new NameGenerator("?" + getIdentifier().name + "ret.", "");
    }

    @Override
    public FIdentifier getIdentifier() {
        return typeVariable.getIdentifier();
    }

    @Override
    public FTypeVariable getType() {
        return typeVariable;
    }

    @Override
    public FIdentifier nextReturnTypeIdentifier() {
        return new FIdentifier(returnTypeNames.next());
    }

    @Override
    public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        if (typeVariable.isResolved())
            return typeVariable.getResolved().getNamespace().softResolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve);

        /* TODO
            on LHS resolve, I can assume returnType VOID, but I need to add another parameter of unknown type (placeholder for assignees)
            and use the same ReturnTypeOf Mechanism I have for that parameter
            At least thats my current best guess...
         */
        if (lhsResolve)
            return Utils.NYI("Lhs resolve on TypeVariable");

        HasCall constraint = new HasSelfCall(null, identifier, positionalArgs, keywordArgs, lhsResolve, this.typeVariable);
        if (!typeVariable.tryAddConstraint(constraint))
            throw new FunctionNotFound(identifier, positionalArgs, keywordArgs);

        //just return some fitting dummy function
        return createDummyResult(identifier, positionalArgs, keywordArgs, returnType, lhsResolve, constraint);
    }

    public FunctionResolver.Result createDummyResult(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve, HasCall constraint) {
        //TODO we might have constraints on the return type, if we are fixed we must have constraints and maybe the return type is fixed as well?
        if (returnType == null)
            returnType = new ReturnTypeOf(nextReturnTypeIdentifier(), typeVariable.isFixed(), positionalArgs, keywordArgs, lhsResolve);
        FFunction f = createDummyFunction(this, identifier, positionalArgs, keywordArgs, returnType);

        FunctionResolver.Result res = new FunctionResolver.Result();
        res.signature = lhsResolve ? f.getLhsSignature() : f.getSignature();
        List<FType> paramTypes = Utils.typesFromExpressionList(res.signature.getParameters());
        res.argMapping = ArgMapping.createBasic(paramTypes, positionalArgs.size());
        res.constraints = ImmutableMultimap.of(typeVariable, constraint);
        return res;
    }

    public static DummyFunction createDummyFunction(Namespace namespace, FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType) {
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> paramsBuilder = ImmutableList.builder();
        for (FType arg : positionalArgs) {
            FIdentifier id = arg == FTypeType.INSTANCE ? new FIdentifier(paramNames.next()) : new FIdentifier(paramNames.next());
            paramsBuilder.add(FParameter.create(id, arg, false));
        }
        for (Map.Entry<FIdentifier, List<FType>> entry : Multimaps.asMap(keywordArgs).entrySet()) {
            paramsBuilder.add(FParameter.create(entry.getKey(), FTuple.from(entry.getValue()), false));
        }
        ImmutableList<FParameter> params = paramsBuilder.build();
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        DummyFunction f = new DummyFunction(identifier, namespace, FVisibilityModifier.EXPORT, false, returnType, params, null, emptyMap());
        if (returnType instanceof ReturnTypeOf)
            ((ReturnTypeOf) returnType).function = f;

        return f;
    }

    @Override
    public FFunction getOpen(FIdentifier identifier) {
        return Utils.NYI("open functions on TypeVariables");
    }

    @Override
    public void addRemoteFunction(FFunction fFunction) {
        Utils.NYI("remote functions on TypeVariables");
    }



    public static class ReturnTypeOf extends FTypeVariable {

        private FFunction function;
        private List<FType> positionalArgs;
        private ListMultimap<FIdentifier, FType> keywordArgs;
        private boolean lhsResolve;

        public ReturnTypeOf(FIdentifier identifier, boolean fixed, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, boolean lhsResolve) {
            super(identifier, fixed, TypeConstraints.create());
            this.positionalArgs = positionalArgs;
            this.keywordArgs = keywordArgs;
            this.lhsResolve = lhsResolve;
        }

        public FTypeVariable getBase() {
            return (FTypeVariable) function.getMemberOf().getType();
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
}
