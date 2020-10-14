package tys.frontier.code.type;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.Multimaps;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.DummyFunction;
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

import java.util.*;

public class FunctionResolver {

    public static class Result {
        public Signature signature;
        public ArgMapping argMapping;
        public ListMultimap<FTypeVariable, TypeConstraint> constraints;
        public int casts;
        public long costs;

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
    private boolean needsReResolve = false;
    private Set<FTypeVariable> typeVariablesThatAreCastToParametersOfOpenBaseCase;

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
        FFunction open = namespace.getOpen(identifier);

        for (final Signature s : namespace.getFunctions(lhsResolve).get(identifier)) {
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

                if (s.getFunction() == open)
                    specialLogicOpen(result, triple.c);

                updateCost(result);
            } catch (IncompatibleTypes | UnfulfillableConstraints | TooManyArguments | NotEnoughArguments ignored) {}
        }

        if (bestResult == null)
            throw new FunctionNotFound(identifier, this.positionalArgs, this.keywordArgs);

        if (typeVariablesThatAreCastToParametersOfOpenBaseCase != null)
            removeConstraintsOpenBaseCase();

        return bestResult;
    }

    private TypeInstantiation computeTypeInstantiation(TypeInstantiation baseInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints, boolean cleanConstraints) throws UnfulfillableConstraints {
        if (baseInstantiation.isEmpty())
            return TypeInstantiation.EMPTY;

        Map<FTypeVariable, FType> typeVariableMap = new HashMap<>();
        ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();

        for (Map.Entry<FTypeVariable, FType> pair : baseInstantiation.entries()) {
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
        if ((!bestResult.constraints.isEmpty() || !newResult.constraints.isEmpty())) {
            if (!bestResult.constraints.keySet().equals(newResult.constraints.keySet())) {
                Utils.NYI("ambiguous function call with constraints");
            }

            ListMultimap<FTypeVariable, TypeConstraint> computedConstraints = MultimapBuilder.hashKeys().arrayListValues().build();

            //check constraints, add HasRemoteCall where necessary
            for (Map.Entry<FTypeVariable, List<TypeConstraint>> entry : Multimaps.asMap(bestResult.constraints).entrySet()) {
                FTypeVariable typeVariable = entry.getKey();
                List<TypeConstraint> bestConstraints = entry.getValue();
                List<TypeConstraint> newConstraints = newResult.constraints.get(typeVariable);

                if (bestConstraints.equals(newConstraints)) {
                    computedConstraints.putAll(typeVariable, bestConstraints);
                    continue;
                }

                needsReResolve = true;
                computedConstraints.put(typeVariable, new HasRemoteCall(null, identifier, positionalArgs, keywordArgs, lhsResolve, namespace));
            }
            bestResult.constraints = computedConstraints;

            if (needsReResolve) {
                createDummyResult(newResult);
                return;
            }
        }

        assert !needsReResolve;
        if (newResult.casts < bestResult.casts || (newResult.casts == bestResult.casts && newResult.costs < bestResult.costs)) {
            bestResult = newResult;
            return;
        }
        if (newResult.casts == bestResult.casts && newResult.costs == bestResult.costs) {
            bestResult = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
        }
    }

    private void createDummyResult(Result newResult) {
        //return some dummy function
        Signature signature = bestResult.signature;
        if (signature.getFunction() instanceof DummyFunction && signature.getFunction() instanceof TypeVariableNamespace.ReturnTypeOf)
            return; //already a Dummy function with most general return type

        FType returnType;
        if (this.returnType != null)
            returnType = this.returnType;
        else if (signature.getType() == newResult.signature.getType())
            returnType = signature.getType();
        else
            returnType = new TypeVariableNamespace.ReturnTypeOf(namespace.nextReturnTypeIdentifier(), false, positionalArgs, keywordArgs, lhsResolve);

        if (signature.getFunction() instanceof DummyFunction && signature.getType() == returnType)
            return;

        FFunction dummyFunction = TypeVariableNamespace.createDummyFunction(namespace, identifier, positionalArgs, keywordArgs, returnType);
        bestResult.signature = lhsResolve ? dummyFunction.getLhsSignature() : dummyFunction.getSignature();
    }

    private void specialLogicOpen(Result result, TypeInstantiation baseInstantiation) {
        typeVariablesThatAreCastToParametersOfOpenBaseCase = new HashSet<>();
        for (Map.Entry<FTypeVariable, List<TypeConstraint>> entry : Multimaps.asMap(result.constraints).entrySet()) {
            FTypeVariable typeVariable = entry.getKey();
            List<TypeConstraint> constraints = entry.getValue();

            if (constraints.size() == 1 && checkTypeConstraintIsFunctionParameter(constraints.get(0), baseInstantiation))
                typeVariablesThatAreCastToParametersOfOpenBaseCase.add(typeVariable);
        }
    }

    private static boolean checkTypeConstraintIsFunctionParameter(TypeConstraint typeConstraint, TypeInstantiation baseInstantiation) {
        if (!(typeConstraint instanceof ImplicitCastable))
            return false;
        ImplicitCastable constraint = (ImplicitCastable) typeConstraint;

        if (!(constraint.getTarget() instanceof FTypeVariable))
            return false;
        FTypeVariable typeVariable = (FTypeVariable) constraint.getTarget();

        return baseInstantiation.values().contains(typeVariable);
    }

    private void removeConstraintsOpenBaseCase() {
        assert needsReResolve || typeVariablesThatAreCastToParametersOfOpenBaseCase.isEmpty();
        for (FTypeVariable typeVariable : typeVariablesThatAreCastToParametersOfOpenBaseCase)
            bestResult.constraints.removeAll(typeVariable);
    }
}
