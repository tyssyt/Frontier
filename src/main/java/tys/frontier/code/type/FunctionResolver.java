package tys.frontier.code.type;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.TypeParameterCast;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ExpressionListToTypeListMapping;
import tys.frontier.util.expressionListToTypeListMapping.ExpressionListToTypeListMapping.NoArgumentsForParameter;
import tys.frontier.util.expressionListToTypeListMapping.ExpressionListToTypeListMapping.TooManyArguments;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

class FunctionResolver {

    public static class Result {
        public FFunction function;
        public Multimap<FTypeVariable, TypeConstraint> constraints;
        public int casts;
        public int costs;

        public static Result perfectFit(FFunction function) {
            Result res = new Result();
            res.function = function;
            res.constraints = ImmutableMultimap.of();
            return res;
        }
    }

    private FFunctionIdentifier identifier;
    private List<FType> positionalArgs;
    private Map<FIdentifier, FType> keywordArgs;
    private FType returnType;

    private Result bestResult;

    public static Result resolve(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, Iterable<FFunction> candidates) throws FunctionNotFound {
        return new FunctionResolver(identifier, positionalArgs, keywordArgs, returnType).resolve(candidates);
    }

    private FunctionResolver(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType) {
        this.identifier = identifier;
        this.positionalArgs = positionalArgs;
        this.keywordArgs = keywordArgs;
        this.returnType = returnType;
    }

    private Result resolve(Iterable<FFunction> candidates) throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
        for (FFunction f : candidates) {
            try {
                ExpressionListToTypeListMapping argMapping = ExpressionListToTypeListMapping.create(positionalArgs, keywordArgs, f.getParams());

                FType argumentTypes = argMapping.getGlue();

                Pair<FFunctionType, TypeInstantiation> pair = FFunctionType.instantiableFrom(f);
                FFunctionType call = FFunctionType.from(argumentTypes, returnType != null ? returnType : pair.a.getOut());
                if (call == pair.a) { //perfect fit
                    return Result.perfectFit(f);
                }

                Result result = new Result();
                result.constraints = ArrayListMultimap.create();

                TypeParameterCast cast = TypeParameterCast.createTPC(call, pair.a, Variance.Contravariant, result.constraints); //this contravariant is hard to explain, but correct

                //compute instantiations
                TypeInstantiation instantiation = computeTypeInstantiation(pair.b, result.constraints, true);
                result.function = f.getInstantiation(instantiation);

                //handle other constraints
                if (TypeConstraints.removeSatisfiableCheckUnsatisfiable(result.constraints) != null)
                    continue;

                result.casts = Utils.countNonNull(cast.getCasts());
                result.costs = cast.getCost();
                updateCost(result);
            } catch (IncompatibleTypes | UnfulfillableConstraints | TooManyArguments | NoArgumentsForParameter ignored) {}
        }

        if (bestResult == null)
            throw new FunctionNotFound(identifier, this.positionalArgs, this.keywordArgs);
        return bestResult;
    }

    private TypeInstantiation computeTypeInstantiation(TypeInstantiation baseInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints, boolean cleanConstraints) throws UnfulfillableConstraints {
        if (baseInstantiation.isEmpty())
            return TypeInstantiation.EMPTY;

        Map<FTypeVariable, FType> typeVariableMap = new HashMap<>();
        Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();

        for (Map.Entry<FTypeVariable, FType> pair : baseInstantiation.getTypeMap().entrySet()) {
            FTypeVariable key = pair.getKey();
            FTypeVariable v = (FTypeVariable) pair.getValue();

            TypeConstraints c = v.getConstraints();
            c = TypeConstraints.addAll(c, constraints.get(v));
            typeVariableMap.put(key, c.softResolve(newConstraints));
            if (cleanConstraints)
                constraints.removeAll(v);
        }
        constraints.putAll(newConstraints);
        return TypeInstantiation.create(typeVariableMap);
    }

    private void updateCost(Result newResult) {
        if (bestResult == null) {
            bestResult = newResult;
            return;
        }
        if (!bestResult.constraints.isEmpty() || !newResult.constraints.isEmpty()) {
            Utils.NYI("ambiguous function call with constraints");
        }

        if (newResult.casts < bestResult.casts || (newResult.casts == bestResult.casts && newResult.costs < bestResult.costs)) {
            bestResult = newResult;
            return;
        }
        if (newResult.casts == bestResult.casts && newResult.costs == bestResult.costs) {
            bestResult = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
        }
    }
}
