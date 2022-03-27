package tys.frontier.code.namespace;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.UnboundExpression;
import tys.frontier.code.function.DummyFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.functionResolve.ArgMatching;
import tys.frontier.code.functionResolve.FunctionResolver;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.NameGenerator;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;

public class TypeVariableNamespace implements Namespace {

    private FTypeVariable typeVariable;
    private NameGenerator returnTypeNames;

    private Location location;

    public TypeVariableNamespace(Location location, FTypeVariable typeVariable) {
        this.location = location;
        this.typeVariable = typeVariable;
        this.returnTypeNames = new NameGenerator("?" + getIdentifier().name + ".ret.", "");
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
    public Location getLocation() {
        return location;
    }

    @Override
    public FIdentifier nextReturnTypeIdentifier() {
        return new FIdentifier(returnTypeNames.next());
    }

    @Override
    public FunctionResolver.Result resolveFunction(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve, List<UnboundExpression> unbounds) throws FunctionNotFound {

        // see if type variable has a constraint that allows this call
        // TODO in theory, since 2 constraints could overload the same function, I should first gather all candidates and then resolve on that!
        for (FClass _class : typeVariable.getImplictCastableCovariant()) {
            try {
                FunctionResolver.Result result = _class.getNamespace().resolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve, unbounds);
                // TODO add a appropriate constraint to the return type variable that casts the correct direction from the return type of 'result'
                // TODO if result is return type void, make the dummy return void
                // TODO think about what stops us from just returning result in our new easier world...
                return createDummyResult(identifier, positionalArgs, keywordArgs, returnType, lhsResolve, unbounds);
            } catch (FunctionNotFound ignored) {}
        }

        throw new FunctionNotFound(identifier, positionalArgs, keywordArgs);
    }

    private FunctionResolver.Result createDummyResult(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve, List<UnboundExpression> unbounds) throws FunctionNotFound {
        //TODO we might have constraints on the return type, if we are fixed we must have constraints and maybe the return type is fixed as well?
        if (returnType == null)
            returnType = new ReturnTypeOf(nextReturnTypeIdentifier(), positionalArgs, keywordArgs, lhsResolve, unbounds);
        FFunction f = createDummyFunction(this, identifier, positionalArgs, keywordArgs, returnType);

        FunctionResolver.Result res = new FunctionResolver.Result();
        res.signature = lhsResolve ? f.getLhsSignature() : f.getSignature();
        res.argMatching = ArgMatching.dummy(positionalArgs.size(), new ArrayList<>(keywordArgs.keySet())); // TODO lets hope that keySet is in the same iteration order as entrySet, otherwise this could get weird
        return res;
    }

    public static DummyFunction createDummyFunction(Namespace namespace, FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType) {
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> paramsBuilder = ImmutableList.builder();
        for (FType arg : positionalArgs) {
            FIdentifier id = new FIdentifier(paramNames.next());
            paramsBuilder.add(FParameter.create(null, id, arg, false));
        }
        for (Map.Entry<FIdentifier, FType> entry : keywordArgs.entrySet()) {
            paramsBuilder.add(FParameter.create(null, entry.getKey(), entry.getValue(), false));
        }
        ImmutableList<FParameter> params = paramsBuilder.build();
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        DummyFunction f = new DummyFunction(identifier, namespace, FVisibilityModifier.EXPORT, null, returnType, params, null, emptyMap());
        if (returnType instanceof ReturnTypeOf)
            ((ReturnTypeOf) returnType).function = f;

        return f;
    }

    public static class ReturnTypeOf extends FTypeVariable {

        private FFunction function;
        private List<FType> positionalArgs;
        private Map<FIdentifier, FType> keywordArgs;
        private boolean lhsResolve;
        private List<UnboundExpression> unbounds;

        public ReturnTypeOf(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, boolean lhsResolve, List<UnboundExpression> unbounds) {
            super(null, identifier); //TODO is there anything reasonable I can use as location?
            this.positionalArgs = positionalArgs;
            this.keywordArgs = keywordArgs;
            this.lhsResolve = lhsResolve;
            this.unbounds = unbounds;
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

        public Map<FIdentifier, FType> getKeywordArgs() {
            return keywordArgs;
        }

        public boolean isLhsResolve() {
            return lhsResolve;
        }

        public List<UnboundExpression> getUnbounds() {
            return unbounds;
        }
    }
}
