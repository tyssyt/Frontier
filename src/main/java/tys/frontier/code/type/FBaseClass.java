package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.NonEmbeddableType;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;

public class FBaseClass extends FClass {

    private FVisibilityModifier constructorVisibility;
    private ImmutableList<FTypeVariable> parametersList;
    private Map<FTypeVariable, Variance> parameterVariance;
    private Map<ImmutableList<FType>, FInstantiatedClass> instantiations;

    private BiMap<FIdentifier, InstanceField> instanceFields = HashBiMap.create();
    private DefaultNamespace namespace;

    private Map<FType, FField> delegates = new HashMap<>();
    private ForImpl forImpl;


    public FBaseClass(Location location, FIdentifier identifier, FVisibilityModifier visibility, NativeDecl nativeDecl) {
        this.parametersList = ImmutableList.of();
        this.parameterVariance = emptyMap();
        this.instantiations = emptyMap();
        this.namespace = new DefaultNamespace(location, identifier, visibility, nativeDecl, this);
    }

    public void addDefaultFunctions() {
        try {
            BinaryOperator.EQUALS_ID.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.NOT_EQUALS_ID.addPredefined(this, FBool.INSTANCE);
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
    public DefaultNamespace getNamespace() {
        return namespace;
    }

    protected void setNamespace(DefaultNamespace namespace) {
        assert this.namespace.isEmpty();
        this.namespace = namespace;
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
    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    @Override
    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    @Override
    public BiMap<FIdentifier, InstanceField> getInstanceFields() {
        return instanceFields;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
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
    public FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments, NonEmbeddableType {
        if (getParametersList().size() != types.size()) {
            throw new WrongNumberOfTypeArguments(this, types, getParametersList().size());
        }
        if (types.size() == 0 || parametersList.equals(types))
            return this;

        ImmutableList<FType> args = ImmutableList.copyOf(types);

        FInstantiatedClass res = instantiations.get(args);
        if (res == null) {
            res = createInstantiatedClass(args);
            instantiations.put(args, res);
            State.get().getCurrentParser().registerInstantiatedClass(res);
        }
        return res;
    }

    protected FInstantiatedClass createInstantiatedClass(ImmutableList<FType> args) {
        return new FInstantiatedClass(new FInstantiatedClassIdentifier(getIdentifier(), args), this, args);
    }

    @Override
    public Map<FType, FField> getDirectDelegates() {
        return delegates;
    }

    @Override
    public boolean isPredefined() {
        return false;
    }
}
