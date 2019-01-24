package tys.frontier.code;

import com.google.common.collect.*;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.cast.TypeConversion.CastType;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.TypeConstraints;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Utils;

import java.util.*;

import static tys.frontier.code.typeInference.Variance.*;

public class FClass implements FType, HasVisibility, HasTypeParameters<FClass> {

    protected FTypeIdentifier identifier;
    protected FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    private Map<FTypeIdentifier, FTypeVariable> parameters;
    private List<FTypeVariable> parametersList;
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

    public void setParameters(Map<FTypeIdentifier, FTypeVariable> parameters) {
        assert this.parameters.isEmpty();
        if (!parameters.isEmpty()) {
            this.parameters = parameters;
            this.parametersList = new ArrayList<>(parameters.values());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();
        }
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

    public FFunction resolveFunction (FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        return new FunctionResolver(identifier, arguments, typeInstantiation).resolve();
    }


    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return parameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return parametersList;
    }

    @Override
    public FClass getInstantiation(TypeInstantiation typeInstantiation) {
        TypeInstantiation intersected = typeInstantiation.intersect(parametersList);
        if (intersected.isEmpty())
            return this;
        return instantiations.computeIfAbsent(typeInstantiation, i -> new FInstantiatedClass(this, intersected));
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

    public void addField (FField field) throws IdentifierCollision {
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

    public void addFunction (FFunction function) throws SignatureCollision {
        for (FFunction other : getFunctions().get(function.getIdentifier())) {
            if (function.getSignature().collidesWith(other.getSignature()))
                throw new SignatureCollision(function, other);
        }
        getFunctions().put(function.getIdentifier(), function);
        uniqueFunctionNames = null;
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

    @Override
    public StringBuilder toString(StringBuilder sb) {
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
        private IntIntPair bestCosts;

        private FFunctionIdentifier identifier;
        private List<FExpression> arguments;
        private TypeInstantiation typeInstantiation;

        FunctionResolver(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) {
            this.identifier = identifier;
            this.arguments = arguments;
            this.typeInstantiation = typeInstantiation;
        }

        /**
         * @param costs pair of number of casts and sum of costs of cast
         */
        void unify(FType arg, FType target, Variance variance, IntIntPair costs, TypeConstraints constraints) throws IncompatibleTypes {
            //ideas: first do the target instanceof FTypeVariabke check, then deal with all class shenaningans
            //when dealing with class shenaningans, in case of contravariance just swap and treat it like covariance
            //note that I can never truely swap because I need to treat the appearance of TypeVariables differently depending which side they are on
            //on the topic of FTypeVariables, when they appear in the arguments treat them as constant types
            //when they appear on side of target, substitute them if they are in typeInstantiation
            //                                    create a constraint if they are in parameters of f
            //

            if (target instanceof FTypeVariable) {
                FType inst = typeInstantiation.getTypeMap().get(target);
                if (inst != null) {
                    unify(arg, inst, Covariant, new IntIntPair(0, 0), constraints); //target is already instantiated (i.e. when resolving in an instantiated class), continue with instantiated type
                } else {
                    //TODO generate more liberal contraints based on whether co/contravariance is okay
                    //constraints.put((FTypeVariable) target, new IsType(arg)); //unification succeeds under the constraint that target is of type arg
                }
                return;
            }

            FType from, to;
            boolean swapped = variance == Contravariant;
            if (swapped) {
                from = target;
                to = arg;
            } else {
                from = arg;
                to = target;
            }

            if (to instanceof FArray) {
                if (!(from instanceof FArray))
                    throw new IncompatibleTypes(target, arg);
                unify(((FArray) arg).getBaseType(), ((FArray) target).getBaseType(), Invariant, costs, constraints);
            } else if (to instanceof FOptional) {
                if (arg instanceof FOptional) {
                    unify(((FOptional) arg).getBaseType(), ((FOptional) target).getBaseType(), variance.then(Covariant), costs, constraints);
                } else {
                    costs.a++;
                    costs.b += CastType.TO_OPTIONAL.baseCost;
                    unify(arg, ((FOptional) target).getBaseType(), variance.then(Covariant), costs, constraints);
                }
            }


            if (target instanceof FOptional) {
                if (arg instanceof FOptional) {
                    unify(((FOptional) arg).getBaseType(), ((FOptional) target).getBaseType(), variance.then(Covariant), costs, constraints);
                } else {
                    switch (variance) {
                        case Covariant:
                            costs.a++;
                            costs.b += CastType.TO_OPTIONAL.baseCost;
                            unify(arg, ((FOptional) target).getBaseType(), variance.then(Covariant), costs, constraints);
                            break;
                        case Contravariant:
                            if (arg == FBool.INSTANCE) {
                                costs.a++;
                                costs.b += CastType.OPTIONAL_TO_BOOL.baseCost;
                            } else
                                throw new IncompatibleTypes(target, arg);
                            break;
                        case Invariant:
                            throw new IncompatibleTypes(target, arg);
                    }
                }
            } else if (target instanceof FFunctionType) {
                if (!(arg instanceof FFunctionType))
                    throw new IncompatibleTypes(target, arg);
                FFunctionType fArg = (FFunctionType) arg;
                FFunctionType fTarget = (FFunctionType) target;
                //fail if functions are of different arity
                if (fArg.getIn().size() != fTarget.getIn().size())
                    throw new IncompatibleTypes(target, arg);
                //unify argument types
                for (int i=0; i<fArg.getIn().size(); i++) {
                    unify(fArg.getIn().get(i), fTarget.getIn().get(i), variance.then(Contravariant), costs, constraints);
                }
                //unify return types
                unify(fArg.getOut(), fTarget.getOut(), variance.then(Covariant), costs, constraints);
            } else if (target instanceof FInstantiatedClass) {
                if (!(arg instanceof FInstantiatedClass))
                    throw new IncompatibleTypes(target, arg);
                //TODO handle delegate casting, however the fuck that might work with generics ???
                FInstantiatedClass iArg = (FInstantiatedClass) arg;
                FInstantiatedClass iTarget = (FInstantiatedClass) target;
                FClass base = iArg.getBaseClass();
                //make sure we have the same base class
                if (base != iTarget.getBaseClass())
                    throw new IncompatibleTypes(target, arg);
                //compare type parameters
                Map<FTypeVariable, FType> argMap = iArg.getTypeInstantiation().getTypeMap();
                Map<FTypeVariable, FType> tagetMap = iTarget.getTypeInstantiation().getTypeMap();
                for (FTypeVariable t : base.getParametersList()) {
                    unify(argMap.get(t), tagetMap.get(t), Invariant, costs, constraints);
                }

            } else if (target instanceof FClass) {
                if (arg != target)
                    throw new IncompatibleTypes(target, arg);
                //TODO try casting (integer & float promotion & delegate, optional2Bool)
            }
        }

        FFunction resolve() throws FunctionNotFound { //TODO for all candidates, store the reason for rejection and use them to generate a better error message
            for (FFunction f : getFunctions().get(identifier)) {
                try {
                    FFunction.Signature sig = f.getSignature();
                    //reject when too many or too few arguments are given
                    if (arguments.size() > sig.getAllParamTypes().size() || arguments.size() < sig.getParamTypes().size())
                        throw new FFunction.IncompatibleSignatures(sig, Utils.typesFromExpressionList(arguments));

                    List<FExpression> args = new ArrayList<>(arguments);
                    //if not enough arguments are given, fill up with default arguments
                    for (int i=args.size(); i<f.getParams().size(); i++) {
                        args.add(f.getParams().get(i).getDefaultValue().get());
                    }

                    TypeConstraints constraints = new TypeConstraints();
                    //iterate over all args and unify types, creating constraints in the process TODO this does not implicit cast on top level (where co-contravariance are still irrelevant)
                    for (int i=0; i<args.size(); i++) {
                        unify(args.get(i).getType(), sig.getAllParamTypes().get(i), Covariant, new IntIntPair(0,0), constraints);
                    }

                    //resolve constraints
                    for (TypeInstantiation instantiation : constraints.solve()) {
                        assert instantiation.fits(f);
                    }

                    //cast arguments & compute cost

                    //update result



                    if (!f.getParametersList().isEmpty()) {
                        //collect constraints

                        //resolve constraints


                    }
                    IntIntPair cost = f.castSignatureFrom(arguments, typeInstantiation);
                    updateCost(cost, f);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, Utils.typesFromExpressionList(arguments));
            return bestFunction;
        }

        private void updateCost(IntIntPair newCosts, FFunction newFunction) {
            if (bestCosts == null) {
                bestCosts = newCosts;
                bestFunction = newFunction;
                return;
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
