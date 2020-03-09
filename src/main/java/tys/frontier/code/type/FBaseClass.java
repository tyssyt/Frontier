package tys.frontier.code.type;

import com.google.common.collect.*;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.InvalidOpenDeclaration;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.*;

public class FBaseClass implements FClass {

    private FIdentifier identifier;
    private FVisibilityModifier visibility;
    private boolean _native;
    private FVisibilityModifier constructorVisibility;
    private ImmutableList<FTypeVariable> parametersList;
    private Map<FTypeVariable, Variance> parameterVariance;
    private Map<ImmutableList<FType>, FInstantiatedClass> instantiations;

    private BiMap<FIdentifier, FField> instanceFields = HashBiMap.create();
    private BiMap<FIdentifier, FField> staticFields = HashBiMap.create();
    private ListMultimap<FIdentifier, Signature> lhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private ListMultimap<FIdentifier, Signature> rhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private Map<FIdentifier, FFunction> openFunctions = new HashMap<>();
    private List<FFunction> remoteFunctions = new ArrayList<>();

    private Map<FType, FField> delegates = new HashMap<>();
    private ForImpl forImpl;

    private NameGenerator lambdaNames = new NameGenerator("λ", "");

    public FBaseClass(FIdentifier identifier, FVisibilityModifier visibility, boolean _native) {
        this.identifier = identifier;
        this.visibility = visibility;
        this._native = _native;
        this.parametersList = ImmutableList.of();
        this.parameterVariance = Collections.emptyMap();
        this.instantiations = Collections.emptyMap();
    }

    protected void addDefaultFunctions() {
        try {
            addFunction(BinaryOperator.EQUALS_ID.createPredefined(this, this, this));
            addFunction(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, this));
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
    public void setForImpl(ForImpl forImpl) {
        this.forImpl = forImpl;
    }

    @Override
    public ForImpl getForImpl() {
        return forImpl;
    }

    @Override
    public long concreteness() {
        return parametersList.isEmpty() ? Long.MAX_VALUE : 1;
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public boolean isNative() {
        return _native;
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
    public ListMultimap<FIdentifier, Signature> getFunctions(boolean lhsSignatures) {
        return lhsSignatures ? lhsFunctions : rhsFunctions;
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
            State.get().getCurrentParser().registerInstantiatedClass(res);
        }
        return res;
    }

    @Override
    public void setOpen(FFunction fFunction) throws InvalidOpenDeclaration {
        if (!this.getParametersList().isEmpty())
            throw new InvalidOpenDeclaration(fFunction, "in generic class");
        if (fFunction.getParameters().isEmpty())
            throw new InvalidOpenDeclaration(fFunction, "non generic function");
        FFunction old = openFunctions.put(fFunction.getIdentifier(), fFunction);
        assert old == null;
    }

    @Override
    public FFunction getOpen(FIdentifier identifier) {
        return openFunctions.get(identifier);
    }

    @Override
    public void addRemoteFunction(FFunction fFunction) {
        remoteFunctions.add(fFunction);
    }

    @Override
    public List<FFunction> getRemoteFunctions() {
        return remoteFunctions;
    }

    @Override
    public Map<FType, FField> getDirectDelegates() {
        return delegates;
    }

    @Override
    public FIdentifier getFreshLambdaName() {
        return new FIdentifier(lambdaNames.next());
    }

    @Override
    public String toString() {
        return tS();
    }

}
