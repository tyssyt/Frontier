package tys.frontier.code.type;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.TypeParameterCast;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class FunctionResolver {

    private FFunction bestFunction;
    private Multimap<FTypeVariable, TypeConstraint> bestConstraints;
    private IntIntPair bestCosts;

    private FFunctionIdentifier identifier;
    private List<FType> argumentTypes;
    private FType returnType;
    private TypeInstantiation typeInstantiation;
    private Iterable<FFunction> candidates;

    FunctionResolver(FFunctionIdentifier identifier, List<FType> argumentTypes, FType returnType, TypeInstantiation typeInstantiation, Iterable<FFunction> candidates) {
        this.identifier = identifier;
        this.argumentTypes = argumentTypes;
        this.typeInstantiation = typeInstantiation;
        this.returnType = returnType;
        this.candidates = candidates;
    }

    Pair<FFunction, Multimap<FTypeVariable, TypeConstraint>> resolve() throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
        for (FFunction f : candidates) {
            try {
                List<FType> argumentTypes = getArgumentTypes(f.getSignature());

                Multimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
                Pair<FFunctionType, TypeInstantiation> pair = FFunctionType.instantiableFrom(f);
                FFunctionType call = FFunctionType.from(argumentTypes, returnType != null ? returnType : pair.a.getOut());
                if (call == pair.a) //perfect fit
                    return new Pair<>(f, ImmutableMultimap.of());

                TypeParameterCast cast = TypeParameterCast.createTPC(call, pair.a, Variance.Contravariant, constraints); //this contravariant is hard to explain, but correct

                //compute instantiations
                TypeInstantiation instantiation = computeTypeInstantiation(pair.b, constraints, true);
                f = f.getInstantiation(instantiation);

                //handle other constraints
                if (TypeConstraints.removeSatisfiableCheckUnsatisfiable(constraints) != null)
                    continue;

                int numberOfCastParameters = Utils.countNonNull(cast.getCasts());
                int castCost = cast.getCost();
                updateCost(new IntIntPair(numberOfCastParameters, castCost), f, constraints);
            } catch (Signature.IncompatibleSignatures | IncompatibleTypes | UnfulfillableConstraints ignored) {}
        }

        if (bestFunction == null)
            throw new FunctionNotFound(identifier, this.argumentTypes);
        return new Pair<>(bestFunction, bestConstraints);
    }

    private List<FType> getArgumentTypes(Signature signature) throws Signature.IncompatibleSignatures {
        //reject when too many or too few arguments are given
        if (argumentTypes.size() > signature.getAllParamTypes().size() || argumentTypes.size() < signature.getParamTypes().size())
            throw new Signature.IncompatibleSignatures(signature, argumentTypes);

        List<FType> argumentTypes = new ArrayList<>(this.argumentTypes); //create a copy that shadows the original argumentTypes, because we do not want to modify them
        //if not enough arguments are given, fill up with default arguments
        for (int i=argumentTypes.size(); i<signature.getAllParamTypes().size(); i++) {
            FType defaultArgType = signature.getAllParamTypes().get(i);
            //default arguments come from the function, thus we might need to instantiate types
            defaultArgType = typeInstantiation.getType(defaultArgType);
            argumentTypes.add(defaultArgType);
        }
        return argumentTypes;
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

    private void updateCost(IntIntPair newCosts, FFunction newFunction, Multimap<FTypeVariable, TypeConstraint> constraints) {
        if (bestCosts == null) {
            bestCosts = newCosts;
            bestFunction = newFunction;
            bestConstraints = constraints;
            return;
        }
        if (!bestConstraints.isEmpty() || !constraints.isEmpty()) {
            Utils.NYI("ambiguous function call with constraints");
        }

        int res = newCosts.compareTo(bestCosts);
        if (res < 0) {
            bestCosts = newCosts;
            bestFunction = newFunction;
        } else if (res == 0) {
            bestFunction = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
        }
    }
}
