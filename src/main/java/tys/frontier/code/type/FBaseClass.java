package tys.frontier.code.type;

import com.google.common.collect.*;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FBaseClass implements FClass {

    private FTypeIdentifier identifier;
    private FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    private ImmutableList<FTypeVariable> parametersList;
    private Map<FTypeVariable, Variance> parameterVariance;
    private Map<ImmutableList<FType>, FInstantiatedClass> instantiations;

    private BiMap<FIdentifier, FField> instanceFields = HashBiMap.create();
    private BiMap<FIdentifier, FField> staticFields = HashBiMap.create();
    private Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    private Map<FType, FField> delegates = new HashMap<>();

    private NameGenerator lambdaNames = new NameGenerator("λ", "");

    public FBaseClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
        this.parametersList = ImmutableList.of();
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

    @Override
    public void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance) {
        assert this.parametersList.isEmpty();
        if (!parameters.isEmpty()) {
            this.parametersList = ImmutableList.copyOf(parameters);
            this.parameterVariance = new HashMap<>(parameters.size());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

            for (int i = 0; i < parameters.size(); i++) {
                FTypeVariable var = parameters.get(i);
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
    public FTypeIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    @Override
    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    @Override
    public BiMap<FIdentifier, FField> getInstanceFields() {
        return instanceFields;
    }

    @Override
    public BiMap<FIdentifier, FField> getStaticFields() {
        return staticFields;
    }

    @Override
    public Multimap<FFunctionIdentifier, FFunction> getFunctions() {
        return functions;
    }

    @Override
    public List<? extends FType> getParametersList() {
        return parametersList;
    }

    @Override
    public Variance getParameterVariance(FTypeVariable parameter) {
        return parameterVariance.get(parameter);
    }

    @Override
    public Variance getParameterVariance(int i) {
        return parameterVariance.get(parametersList.get(i));
    }

    @Override
    public FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments {
        if (getParametersList().size() != types.size()) {
            throw new WrongNumberOfTypeArguments(this, types, getParametersList().size());
        }
        if (types.size() == 0 || parametersList.equals(types))
            return this;

        ImmutableList<FType> args = ImmutableList.copyOf(types);

        FInstantiatedClass res = instantiations.get(args);
        if (res == null) {
            res = new FInstantiatedClass(this, args);
            instantiations.put(args, res);
            res.prepare();
        }
        return res;
    }

    @Override
    public Map<FType, FField> getDirectDelegates() {
        return delegates;
    }

    //TODO yeah that is not clean code, I know...
    static boolean getDelegate(FClass _this, FType toType, List<FField> res) {
        FField f = _this.getDirectDelegates().get(toType);
        if (f != null) {
            res.add(f);
            return true;
        }

        for (Map.Entry<FType, FField> entry : _this.getDirectDelegates().entrySet()) {
            res.add(entry.getValue());
            if (entry.getKey() instanceof FClass && getDelegate((FClass) entry.getKey(), toType, res))
                return true;
            res.remove(res.size() - 1);
        }
        return false;
    }

    @Override
    public FFunctionIdentifier getFreshLambdaName() {
        return new FFunctionIdentifier(lambdaNames.next());
    }

    @Override
    public String toString() {
        return tS();
    }

}