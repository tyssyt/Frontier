package tys.frontier.code;

import com.google.common.collect.*;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

public class FClass implements FType, HasVisibility, HasTypeParameters<FClass> {

    protected FTypeIdentifier identifier;
    protected FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    private Map<FTypeIdentifier, FTypeVariable> parameters;
    private List<FTypeVariable> parametersList;
    private Map<FTypeVariable, Variance> parameterVariance;
    private Map<TypeInstantiation, FInstantiatedClass> instantiations;

    protected BiMap<FIdentifier, FField> instanceFields = HashBiMap.create();
    protected BiMap<FIdentifier, FField> staticFields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    protected Map<FType, FField> delegates = new HashMap<>();

    private NameGenerator lambdaNames = new NameGenerator("λ", "");
    protected Map<FFunction, String> uniqueFunctionNames;

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
        this.parameters = Collections.emptyMap();
        this.parametersList = Collections.emptyList();
        this.parameterVariance = Collections.emptyMap();
        this.instantiations = Collections.emptyMap();
    }

    protected void addDefaultFunctions() {
        try {
            addFunction(FBinaryOperator.Bool.EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS_ID.createPredefined(this));
        } catch (SignatureCollision e) {
            Utils.handleException(e);
        }
    }

    public void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance) {
        assert this.parameters.isEmpty();
        if (!parameters.isEmpty()) {
            this.parameters = new HashMap<>(parameters.size());
            this.parametersList = parameters;
            this.parameterVariance = new HashMap<>(parameters.size());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

            for (int i = 0; i < parameters.size(); i++) {
                FTypeVariable var = parameters.get(i);
                this.parameters.put(var.getIdentifier(), var);
                this.parameterVariance.put(var, parameterVariance.get(i));
            }
        }
    }

    @Override
    public long concreteness() {
        return parametersList.isEmpty() ? Long.MAX_VALUE : 1;
    }

    @Override
    public boolean canImplicitlyCast() {
        return Utils.cantHappen();
    }

    @Override
    public FTypeIdentifier getIdentifier () {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    @Override
    public FField getField(FIdentifier identifier) throws FieldNotFound {
        FField f = instanceFields.get(identifier);
        if (f != null)
            return f;
        f = staticFields.get(identifier);
        if (f != null)
            return f;
        throw new FieldNotFound(identifier);
    }

    public BiMap<FIdentifier, FField> getInstanceFields() {
        return instanceFields;
    }

    public BiMap<FIdentifier, FField> getStaticFields() {
        return staticFields;
    }

    public Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    public Multimap<FFunctionIdentifier, FFunction> getFunctions() {
        return functions;
    }

    @Override
    public FFunction resolveFunction (FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        Pair<FFunction, Multimap<FTypeVariable, TypeConstraint>> pair = new FunctionResolver(identifier, argumentTypes, typeInstantiation).resolve();
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : pair.b.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new FunctionNotFound(identifier, argumentTypes);
        }
        return pair.a;
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound {
        Pair<FFunction, Multimap<FTypeVariable, TypeConstraint>> pair = new FunctionResolver(identifier, argumentTypes, typeInstantiation).resolve();
        constraints.putAll(pair.b);
        return pair.a;
    }

    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return parameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return parametersList;
    }

    public Variance getParameterVariance(FTypeVariable parameter) {
        return parameterVariance.get(parameter);
    }

    @Override
    public FClass getInstantiation(TypeInstantiation typeInstantiation) {
        TypeInstantiation intersected = typeInstantiation.intersect(parametersList);
        if (intersected.isEmpty())
            return this;
        FInstantiatedClass res = instantiations.get(intersected);
        if (res == null) {
            res = new FInstantiatedClass(this, intersected);
            instantiations.put(intersected, res);
            res.prepare();
        }
        return res;
    }

    public void addDelegate(FField field) {
        assert field.getMemberOf() == this;
        assert field.getType() instanceof FClass;
        if (field.getVisibility() != FVisibilityModifier.PRIVATE)
            delegates.put(field.getType(), field);
    }

    public List<FField> getDelegate(FType toType) {
        List<FField> res = new ArrayList<>();
        if (getDelegate(toType, res))
            return res;
        else
            return null;
    }

    private boolean getDelegate(FType toType, List<FField> res) {
        FField f = delegates.get(toType);
        if (f != null) {
            res.add(f);
            return true;
        }

        for (Map.Entry<FType, FField> entry : delegates.entrySet()) {
            res.add(entry.getValue());
            if (entry.getKey() instanceof FClass && ((FClass) entry.getKey()).getDelegate(toType, res))
                return true;
            res.remove(res.size() - 1);
        }
        return false;
    }

    public void addField(FField field) throws IdentifierCollision {
        if (field.isInstance()) {
            FField old = getInstanceFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        } else {
            FField old = getStaticFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        }
    }

    public void addFieldTrusted(FField field) {
        try {
            addField(field);
        } catch (IdentifierCollision identifierCollision) {
            Utils.cantHappen();
        }
    }

    public void addFunction(FFunction function) throws SignatureCollision {
        for (FFunction other : getFunctions().get(function.getIdentifier())) {
            if (function.getSignature().collidesWith(other.getSignature()))
                throw new SignatureCollision(function, other);
        }
        getFunctions().put(function.getIdentifier(), function);
        uniqueFunctionNames = null;
    }

    public void addFunctionTrusted(FFunction function) {
        try {
            addFunction(function);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getFunctions().get(FConstructor.IDENTIFIER));
    }

    public FConstructor generateConstructor() {
        FVisibilityModifier visibility = constructorVisibility == null ? FVisibilityModifier.PRIVATE : constructorVisibility;
        try {
            addFunction(FConstructor.createMalloc(this));
            FConstructor res = FConstructor.create(visibility, this);
            addFunction(res);
            return res;
        } catch (SignatureCollision signatureCollision) {
            return Utils.handleException(signatureCollision);
        }
    }

    public Map<FFunction, String> getUniqueFunctionNames() {
        if (uniqueFunctionNames == null) {
            uniqueFunctionNames = computeUniqueFunctionNames();
        }
        return uniqueFunctionNames;
    }

    public FFunctionIdentifier getFreshLambdaName() {
        return new FFunctionIdentifier(lambdaNames.next());
    }

    private Map<FFunction, String> computeUniqueFunctionNames() {
        Map<FFunction, String> res = new HashMap<>();
        ArrayListMultimap<FFunctionIdentifier, FFunction> allFuncs = ArrayListMultimap.create(getFunctions());
        for (Collection<FFunction> coll : allFuncs.asMap().values()) {
            List<FFunction> list = ((List<FFunction>) coll);
            String name = list.get(0).getIdentifier().name;

            if (list.size() == 1) {
                res.put(list.get(0), name);
                continue;
            }

            list.sort((f1, f2) -> {
                int c = f1.getParams().size() - f2.getParams().size();
                if (c != 0)
                    return c;
                for (int i=0; i<f1.getParams().size(); i++) {
                    String id1 = f1.getParams().get(i).getType().getIdentifier().name;
                    String id2 = f2.getParams().get(i).getType().getIdentifier().name;
                    c = id1.compareTo(id2);
                    if (c != 0)
                        return c;
                }
                return 0;
            });
            for (int i=0; i<list.size(); i++) {
                res.put(list.get(i), name + "#" + i);
            }
        }
        return res;
    }

    public void removeUnreachable(Reachability.ReachableClass reachable) {
        getStaticFields().values().retainAll(reachable.reachableFields);
        getInstanceFields().values().retainAll(reachable.reachableFields);
        getFunctions().values().retainAll(reachable.reachableFunctions.keySet());
    }

    public <C,Fi,Fu,S,E> C accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterType(this);
        List<Fi> fields = new ArrayList<>(this.getInstanceFields().size() + this.getStaticFields().size());
        for (FField f : this.getInstanceFields().values()) {
            fields.add(f.accept(visitor));
        }
        for (FField f : this.getStaticFields().values()) {
            fields.add(f.accept(visitor));
        }
        List<Fu> functions = new ArrayList<>(this.getFunctions().size());
        for (FFunction f : this.getFunctions().values()) {
            functions.add(f.accept(visitor));
        }
        return visitor.exitType(this, fields, functions);
    }

    @Override
    public String toString() {
        return tS();
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }

    public String headerToString() {
        return visibility + " class " + identifier;
    }

    public StringBuilder summary(StringBuilder sb) {
        sb.append(headerToString()).append("{\n  ");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append(", ");
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append(", ");
        }
        sb.append("\n  ");
        for (FFunction function : getFunctions().values()) {
            sb.append(function.headerToString()).append(", ");
        }
        return sb.append("\n}");
    }

    public StringBuilder printAll(StringBuilder sb) {
        sb.append(headerToString()).append("{\n");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FFunction function : getFunctions().values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }

    private class FunctionResolver {
        private FFunction bestFunction;
        private Multimap<FTypeVariable, TypeConstraint> bestConstraints;
        private IntIntPair bestCosts;

        private FFunctionIdentifier identifier;
        private List<FType> argumentTypes;
        private TypeInstantiation typeInstantiation;

        FunctionResolver(FFunctionIdentifier identifier, List<FType> argumentTypes, TypeInstantiation typeInstantiation) {
            this.identifier = identifier;
            this.argumentTypes = argumentTypes;
            this.typeInstantiation = typeInstantiation;
        }

        Pair<FFunction, Multimap<FTypeVariable, TypeConstraint>> resolve() throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
            for (FFunction f : getFunctions().get(identifier)) {
                try {
                    List<FType> argumentTypes = getArgumentTypes(f.getSignature());

                    f = f.getInstantiableCopy();

                    Multimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
                    IntIntPair cost = f.castSignatureFrom(argumentTypes, typeInstantiation, constraints);

                    //compute instantiations
                    TypeInstantiation instantiation = computeTypeInstantiation(f, constraints, true);
                    f = f.getInstantiation(instantiation);

                    //handle other constraints
                    if (TypeConstraints.removeSatisfiableCheckUnsatisfiable(constraints) != null)
                        continue;

                    updateCost(cost, f, constraints);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes | UnfulfillableConstraints ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, this.argumentTypes);
            return new Pair<>(bestFunction, bestConstraints);
        }

        private List<FType> getArgumentTypes(FFunction.Signature signature) throws FFunction.IncompatibleSignatures {
            //reject when too many or too few arguments are given
            if (argumentTypes.size() > signature.getAllParamTypes().size() || argumentTypes.size() < signature.getParamTypes().size())
                throw new FFunction.IncompatibleSignatures(signature, argumentTypes);

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

        private TypeInstantiation computeTypeInstantiation(FFunction toInstantiate, Multimap<FTypeVariable, TypeConstraint> constraints, boolean cleanConstraints) throws UnfulfillableConstraints {
            if (toInstantiate.getParametersList().isEmpty())
                return TypeInstantiation.EMPTY;
            
            Map<FTypeVariable, FType> typeVariableMap = new HashMap<>();
            Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();

            for (FTypeVariable v : toInstantiate.getParametersList()) {
                TypeConstraints c = v.getConstraints();
                c = TypeConstraints.addAll(c, constraints.get(v));
                Pair<FType, Multimap<FTypeVariable, TypeConstraint>> pair = c.resolve();
                typeVariableMap.put(v, pair.a);
                newConstraints.putAll(pair.b);
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

}
