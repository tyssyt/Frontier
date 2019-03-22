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


    private Map<FType, FField> delegates = new HashMap<>();

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
    public boolean isFullyInstantiated() {
        return parametersList.isEmpty();
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
        getFunctions().values().retainAll(reachable.reachableFunctions);
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
            functionLoop: for (FFunction f : getFunctions().get(identifier)) {
                try {
                    FFunction.Signature sig = f.getSignature();
                    //reject when too many or too few arguments are given
                    if (argumentTypes.size() > sig.getAllParamTypes().size() || argumentTypes.size() < sig.getParamTypes().size())
                        throw new FFunction.IncompatibleSignatures(sig, argumentTypes); //why not just continue the loop if we ignore the error anyway?

                    List<FType> argumentTypes = new ArrayList<>(this.argumentTypes); //create a copy that shadows the original argumentTypes, because we do not want to modify them
                    //if not enough arguments are given, fill up with default arguments
                    for (int i=this.argumentTypes.size(); i<f.getParams().size(); i++) {
                        FType defaultArgType = f.getParams().get(i).getType();
                        //default arguments come from the function, thus we might need to instantiate types
                        defaultArgType = typeInstantiation.getType(defaultArgType);
                        argumentTypes.add(defaultArgType);
                    }

                    Multimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
                    IntIntPair cost = f.castSignatureFrom(argumentTypes, typeInstantiation, constraints);

                    //compute instantiations
                    Map<FTypeVariable, FType> typeVarianleMap = new HashMap<>();
                    Multimap<FTypeVariable, TypeConstraint> newConstraints = ArrayListMultimap.create();
                    for (Map.Entry<FTypeVariable, Collection<TypeConstraint>> entry : constraints.asMap().entrySet()) {
                        FTypeVariable typeVariable = entry.getKey();

                        if (f.getParameters().get(typeVariable.getIdentifier()) == typeVariable) { //typeVariable is a Parameter
                            if (!typeVariable.isFixed() && !f.getBody().isPresent())
                                return Utils.NYI("calling a function with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing

                            TypeConstraints copy = typeVariable.getConstraints().copy();
                            copy.addAll(entry.getValue());
                            typeVarianleMap.put(typeVariable, copy.resolve(newConstraints));
                        }
                    }
                    TypeInstantiation instantiation = TypeInstantiation.create(typeVarianleMap);
                    assert instantiation.fits(f);
                    for (FTypeVariable v : f.getParametersList()) {
                        constraints.removeAll(v);
                    }
                    constraints.putAll(newConstraints);
                    f = f.getInstantiation(instantiation);

                    //handle other constraints
                    Iterator<Map.Entry<FTypeVariable, TypeConstraint>> it = constraints.entries().iterator();
                    while (it.hasNext()) {
                        Map.Entry<FTypeVariable, TypeConstraint> entry = it.next();
                        if (entry.getKey().getConstraints().satisfies(entry.getValue())) {
                            it.remove(); //remove all satisfied constraints
                            continue;
                        }
                        if (entry.getKey().isFixed()) {
                            continue functionLoop; //constraint is unsatisfiable f can't be called
                        }
                        //otherwise the constraint stays in the set
                    }

                    updateCost(cost, f, constraints);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes | UnfulfillableConstraints ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, this.argumentTypes);
            return new Pair<>(bestFunction, bestConstraints);
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
