package tys.frontier.code.type;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
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
import tys.frontier.util.Triple;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping.NoArgumentsForParameter;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping.TooManyArguments;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FunctionResolver {

    public static class Result {
        public FFunction function;
        public ArgMapping argMapping;
        public Multimap<FTypeVariable, TypeConstraint> constraints;
        public int casts;
        public int costs;
    }

    private FFunctionIdentifier identifier;
    private List<FType> positionalArgs;
    private ListMultimap<FIdentifier, FType> keywordArgs;
    private FType returnType;

    private Result bestResult;

    public static Result resolve(FFunctionIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, Iterable<FFunction> candidates) throws FunctionNotFound {
        return new FunctionResolver(identifier, positionalArgs, keywordArgs, returnType).resolve(candidates);
    }

    private FunctionResolver(FFunctionIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType) {
        this.identifier = identifier;
        this.positionalArgs = positionalArgs;
        this.keywordArgs = keywordArgs;
        this.returnType = returnType;
    }

    private Result resolve(Iterable<FFunction> candidates) throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
        for (FFunction f : candidates) {
            try {
                Result result = new Result();
                //pack/unpack tuples, map keyword Args and use default parameters
                Pair<ArgMapping, List<FType>> argMappingAndArgumentTypes = ArgMapping.createForCall(positionalArgs, keywordArgs, f.getParams());
                result.argMapping = argMappingAndArgumentTypes.a;
                //prepare f
                Triple<List<FType>, FType, TypeInstantiation> triple = FFunctionType.instantiableFrom(f); //TODO unbutcher this?
                //cast arguments
                result.constraints = result.argMapping.computeCasts(argMappingAndArgumentTypes.b, triple.a);

                //check for return Type if specified
                if (returnType != null && returnType != triple.b) {
                    ImplicitTypeCast.create(returnType, triple.b, Variance.Contravariant, result.constraints);
                }

                //fast path for perfect fit
                result.casts = result.argMapping.getNUmberOfCasts();
                if (result.casts == 0 && result.constraints.isEmpty()) {
                    result.function = f;
                    return result; //perfect fit
                }

                //compute instantiations
                TypeInstantiation instantiation = computeTypeInstantiation(triple.c, result.constraints, true);
                result.function = f.getInstantiation(instantiation);

                //handle other constraints
                if (TypeConstraints.removeSatisfiableCheckUnsatisfiable(result.constraints) != null)
                    continue;

                //recompute casts TODO this needs adapted once we allow generic functions to be instantiated with tuples
                result.argMapping.computeCasts(argMappingAndArgumentTypes.b, Utils.typesFromExpressionList(result.function.getParams()));

                result.costs = result.argMapping.getCostsOfCasts();
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
