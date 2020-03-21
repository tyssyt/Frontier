package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.*;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.ForPlaceholder;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FInstantiatedClass extends FForwardingClass {

    private FInstantiatedClassIdentifier newIdentifier;
    private boolean baked = false;
    private ImmutableList<FType> instantiatedParameters;
    private Map<FFunction, FFunction> baseFunctionMap = new HashMap<>();

    private BiMap<FIdentifier, FField> newInstanceFields = HashBiMap.create();
    private BiMap<FIdentifier, FField> newStaticFields = HashBiMap.create();

    private DefaultNamespace newNamespace;

    private Map<FType, FField> newDelegates;

    private ForImpl newForImpl;

    FInstantiatedClass(FClass baseClass, ImmutableList<FType> instantiatedParameters) {
        super(baseClass);
        assert !(baseClass instanceof FInstantiatedClass);
        assert instantiatedParameters.size() == baseClass.getParametersList().size();
        newIdentifier = new FInstantiatedClassIdentifier(baseClass.getIdentifier(), instantiatedParameters);
        this.instantiatedParameters = instantiatedParameters;
        this.newNamespace = new DefaultNamespace(this);
    }

    @Override
    public FIdentifier getIdentifier() {
        return newIdentifier;
    }

    public void prepare() {
        TypeInstantiation typeInstantiation = getTypeInstantiation();
        //add fields
        for (FField baseField : proxy.getFields()) {
            FField instantiatedField = new FField(baseField.getIdentifier(), typeInstantiation.getType(baseField.getType()),
                    this, baseField.getVisibility(), !baseField.isInstance(), baseField.hasAssignment());
            this.addFieldTrusted(instantiatedField);
            baseFunctionMap.put(baseField.getGetter(), instantiatedField.getGetter());
            baseFunctionMap.put(baseField.getSetter(), instantiatedField.getSetter());
        }

        //delegates
        newDelegates = new HashMap<>();
        for (Map.Entry<FType, FField> entry : proxy.getDirectDelegates().entrySet()) {
            newDelegates.put(typeInstantiation.getType(entry.getKey()), newInstanceFields.get(entry.getValue().getIdentifier()));
        }

        //add functions
        for (Signature base : proxy.getNamespace().getFunctions(false).values()) {
            FFunction baseFunction = base.getFunction();
            if (baseFunction.isConstructor() || baseFunction instanceof FieldAccessor || baseFunction.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = ClassInstantiationFunction.fromClassInstantiation(this, baseFunction);
            baseFunctionMap.put(baseFunction, instantiatedFunction);
            this.newNamespace.addFunctionTrusted(instantiatedFunction);
        }

        //constructor
        setConstructorVisibility(proxy.getConstructorVisibility());
        FConstructor constructor = generateConstructor();
        baseFunctionMap.put(proxy.getConstructor(), constructor);
        //TODO do we need to put malloc in the map?

        //forImpl
        if (proxy.getForImpl() != null)
            newForImpl = ForPlaceholder.INSTANCE;
    }

    public FFunction getInstantiatedFunction(FFunction baseFunction) {
        return baseFunctionMap.get(baseFunction);
    }

    public TypeInstantiation getTypeInstantiation() {
        Map<FTypeVariable, FType> typeInstantiation = new HashMap<>();
        //noinspection unchecked
        List<FTypeVariable> baseParameters = (List<FTypeVariable>) proxy.getParametersList();
        for (int i = 0; i < instantiatedParameters.size(); i++)
            typeInstantiation.put(baseParameters.get(i), instantiatedParameters.get(i));
        return TypeInstantiation.create(typeInstantiation);
    }

    @Override
    public void setForImpl(ForImpl forImpl) {
        Utils.cantHappen();
    }

    @Override
    public ForImpl getForImpl() {
        if (newForImpl == ForPlaceholder.INSTANCE) { //lazy instantiation
            assert proxy.getForImpl() instanceof ForByIdx;
            ForByIdx forImpl = (ForByIdx) proxy.getForImpl();
            newForImpl = new ForByIdx(baseFunctionMap.get(forImpl.getGetElement()), baseFunctionMap.get(forImpl.getGetSize()));
        }
        return newForImpl;
    }

    @Override
    public List<FType> getParametersList() {
        return instantiatedParameters;
    }

    @Override
    public long concreteness() {
        long res = Long.MAX_VALUE;
        for (FType param : instantiatedParameters) {
            res = Long.min(res, param.concreteness());
        }
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }

    @Override
    public Map<FType, FField> getDirectDelegates() {
        return newDelegates;
    }

    @Override
    public BiMap<FIdentifier, FField> getInstanceFields() {
        return newInstanceFields;
    }

    @Override
    public BiMap<FIdentifier, FField> getStaticFields() {
        return newStaticFields;
    }

    @Override
    public DefaultNamespace getNamespace() {
        return newNamespace;
    }

    public boolean isBaked() {
        return baked;
    }

    public void bake() {
        assert !baked;
        GenericBaking.bake(this);
        baked = true;
    }
}
