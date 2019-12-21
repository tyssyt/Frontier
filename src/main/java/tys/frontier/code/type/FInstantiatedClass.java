package tys.frontier.code.type;

import com.google.common.collect.*;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.ClassInstantiationFunction;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.passes.GenericBaking;

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
    private ListMultimap<FFunctionIdentifier, Signature> newLhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private ListMultimap<FFunctionIdentifier, Signature> newRhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();

    private Map<FType, FField> newDelegates;

    FInstantiatedClass(FClass baseClass, ImmutableList<FType> instantiatedParameters) {
        super(baseClass);
        assert !(baseClass instanceof FInstantiatedClass);
        assert instantiatedParameters.size() == baseClass.getParametersList().size();
        newIdentifier = new FInstantiatedClassIdentifier(baseClass.getIdentifier(), instantiatedParameters);
        this.instantiatedParameters = instantiatedParameters;
    }

    @Override
    public FTypeIdentifier getIdentifier() {
        return newIdentifier;
    }

    public void prepare() {
        TypeInstantiation typeInstantiation = getTypeInstantiation();
        //add fields
        for (FField baseField : proxy.getFields()) {
            FField instantiatedField = new FField(baseField.getIdentifier(), typeInstantiation.getType(baseField.getType()),
                    this, baseField.getVisibility(), !baseField.isInstance(), baseField.hasAssignment());
            this.addFieldTrusted(instantiatedField);
        }

        //delegates
        newDelegates = new HashMap<>();
        for (Map.Entry<FType, FField> entry : proxy.getDirectDelegates().entrySet()) {
            newDelegates.put(typeInstantiation.getType(entry.getKey()), newInstanceFields.get(entry.getValue().getIdentifier()));
        }

        //add functions
        for (Signature base : proxy.getFunctions(false).values()) {
            FFunction baseFunction = base.getFunction();
            if (baseFunction.isConstructor() || baseFunction.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = ClassInstantiationFunction.fromClassInstantiation(this, baseFunction);
            baseFunctionMap.put(baseFunction, instantiatedFunction);
            this.addFunctionTrusted(instantiatedFunction);
        }

        //constructor
        setConstructorVisibility(proxy.getConstructorVisibility());
        FConstructor constructor = generateConstructor();
        baseFunctionMap.put(proxy.getConstructor(), constructor);
        //TODO do we need to put malloc in the map?
    }

    public FFunction getInstantiatedFunction(FFunction baseFunction) {
        assert baseFunction.getMemberOf() == proxy;
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
    public ListMultimap<FFunctionIdentifier, Signature> getFunctions(boolean lhsSignatures) {
        return lhsSignatures ? newLhsFunctions : newRhsFunctions;
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
