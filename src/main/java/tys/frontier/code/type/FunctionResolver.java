package tys.frontier.code.type;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.typeInference.*;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Triple;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.collect.Iterables.getOnlyElement;

public class FunctionResolver {

    public static class Result {
        public Signature signature;
        public ArgMapping argMapping;
        public Multimap<FTypeVariable, TypeConstraint> constraints;
        public int casts;
        public int costs;

        public FFunction getFunction() {
            return signature.getFunction();
        }
    }

    private FIdentifier identifier;
    private List<FType> positionalArgs;
    private ListMultimap<FIdentifier, FType> keywordArgs;
    private FType returnType;
    private DefaultNamespace namespace;
    private boolean lhsResolve;

    private Result bestResult;

    public static Result resolve(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, DefaultNamespace namespace, boolean lhsResove) throws FunctionNotFound {
        return new FunctionResolver(identifier, positionalArgs, keywordArgs, returnType, namespace, lhsResove).resolve();
    }

    private FunctionResolver(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, DefaultNamespace namespace, boolean lhsResolve) {
        this.identifier = identifier;
        this.positionalArgs = positionalArgs;
        this.keywordArgs = keywordArgs;
        this.returnType = returnType;
        this.namespace = namespace;
        this.lhsResolve = lhsResolve;
    }

    private Result resolve() throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
        for (Signature s : namespace.getFunctions(lhsResolve).get(identifier)) {
            try {
                Result result = new Result();
                //pack/unpack tuples, map keyword Args and use default parameters
                Pair<ArgMapping, List<FType>> argMappingAndArgumentTypes = ArgMapping.createForCall(positionalArgs, keywordArgs, s.getParameters());
                result.argMapping = argMappingAndArgumentTypes.a;
                //prepare f
                Triple<List<FType>, FType, TypeInstantiation> triple = FFunctionType.instantiableFrom(s); //TODO unbutcher this?
                //cast arguments
                result.constraints = result.argMapping.computeCasts(argMappingAndArgumentTypes.b, triple.a);

                //check for return Type if specified
                if (returnType != null && returnType != triple.b) {
                    ImplicitTypeCast.create(returnType, triple.b, Variance.Contravariant, result.constraints);
                }

                //fast path for perfect fit
                result.casts = result.argMapping.getNUmberOfCasts();
                result.costs = result.argMapping.getCostsOfCasts();
                if (result.casts == 0 && result.constraints.isEmpty()) {
                    result.signature = s;
                    return result; //perfect fit
                }

                //compute instantiations
                TypeInstantiation instantiation = computeTypeInstantiation(triple.c, result.constraints, true);
                result.signature = s.getInstantiation(instantiation);

                //handle other constraints
                if (TypeConstraints.removeSatisfiableCheckUnsatisfiable(result.constraints) != null)
                    continue;

                //recompute casts TODO this needs adapted once we allow generic functions to be instantiated with tuples
                result.argMapping.computeCasts(argMappingAndArgumentTypes.b, Utils.typesFromExpressionList(result.signature.getParameters()));

                updateCost(result);
            } catch (IncompatibleTypes | UnfulfillableConstraints | TooManyArguments | NotEnoughArguments ignored) {}
        }

        if (bestResult == null)
            throw new FunctionNotFound(identifier, this.positionalArgs, this.keywordArgs);
        return bestResult;
    }

    private TypeInstantiation computeTypeInstantiation(TypeInstantiation baseInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints, boolean cleanConstraints) throws UnfulfillableConstraints {
        if (baseInstantiation.isEmpty())
            return TypeInstantiation.EMPTY;

        Map<FTypeVariable, FType> typeVariableMap = new HashMap<>();
        ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();

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
        if ((!bestResult.constraints.isEmpty() || !newResult.constraints.isEmpty()) && !bestResult.constraints.equals(newResult.constraints)) {
            Multimap<FTypeVariable, TypeConstraint> computedConstraints = MultimapBuilder.hashKeys().arrayListValues(1).build();
            if (bestResult.constraints.keySet().equals(newResult.constraints.keySet())) {

                for (Map.Entry<FTypeVariable, Collection<TypeConstraint>> entry : bestResult.constraints.asMap().entrySet()) {
                    FTypeVariable typeVariable = entry.getKey();
                    Collection<TypeConstraint> bestConstraints = entry.getValue();
                    Collection<TypeConstraint> newConstraints = newResult.constraints.get(typeVariable);

                    if (bestConstraints.size() != 1 || newConstraints.size() != 1
                            || !(getOnlyElement(bestConstraints) instanceof ImplicitCastable || getOnlyElement(bestConstraints) instanceof HasRemoteCall)
                            || !(getOnlyElement(newConstraints)  instanceof ImplicitCastable || getOnlyElement(newConstraints)  instanceof HasRemoteCall)
                    ) {
                        Utils.NYI("ambiguous function call with constraints");
                        return;
                    }

                    computedConstraints.put(typeVariable, new HasRemoteCall(null, identifier, positionalArgs, keywordArgs, lhsResolve, namespace));

                    //return some dummy function
                    FType returnType;
                    if (this.returnType != null)
                        returnType = this.returnType;
                    else if (bestResult.signature.getType() == newResult.signature.getType())
                        returnType = newResult.signature.getType();
                    else
                        returnType = TypeVariableNamespace.ReturnTypeOf.create(namespace.nextReturnTypeIdentifier(), false); //TODO no idea what to set fixed to

                    FFunction dummyFunction = TypeVariableNamespace.createDummyFunction(namespace, identifier, positionalArgs, keywordArgs, returnType);
                    bestResult.signature = lhsResolve ? dummyFunction.getLhsSignature() : dummyFunction.getSignature();

                }
                bestResult.constraints = computedConstraints;
                return;
            }
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
