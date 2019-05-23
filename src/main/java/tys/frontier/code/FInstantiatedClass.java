package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.function.ClassInstantiationFunction;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.passes.GenericBaking;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FInstantiatedClass extends FClass {

    private boolean baked = false;
    private FClass baseClass;
    private ImmutableList<FType> instantiatedParameters;
    private Map<FFunction, FFunction> baseFunctionMap = new HashMap<>();

    FInstantiatedClass(FClass baseClass, ImmutableList<FType> instantiatedParameters) {
        super(new FInstantiatedClassIdentifier(baseClass.getIdentifier(), instantiatedParameters), baseClass.getVisibility());
        assert !(baseClass instanceof FInstantiatedClass);
        assert instantiatedParameters.size() == baseClass.getParametersList().size();
        this.baseClass = baseClass;
        this.instantiatedParameters = instantiatedParameters;
    }

    void prepare() {
        TypeInstantiation typeInstantiation = getTypeInstantiation();
        //add fields
        for (FField baseField : baseClass.getFields()) {
            FField instantiatedField = new FField(baseField.getIdentifier(), typeInstantiation.getType(baseField.getType()),
                    this, baseField.getVisibility(), !baseField.isInstance(), baseField.hasAssignment());
            this.addFieldTrusted(instantiatedField);
        }

        //add functions
        for (FFunction baseFunction : baseClass.getFunctions().values()) {
            if (baseFunction.isConstructor() || baseFunction.getIdentifier() == FConstructor.MALLOC_ID)
                continue;
            ClassInstantiationFunction instantiatedFunction = ClassInstantiationFunction.fromClassInstantiation(this, baseFunction);
            baseFunctionMap.put(baseFunction, instantiatedFunction);
            this.addFunctionTrusted(instantiatedFunction);
        }

        //constructor
        setConstructorVisibility(baseClass.getConstructorVisibility());
        FConstructor constructor = generateConstructor();
        baseFunctionMap.put(baseClass.getConstructor(), constructor);
        //TODO do we need to put malloc in the map?
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    public FFunction getInstantiatedFunction(FFunction baseFunction) {
        assert baseFunction.getMemberOf() == baseClass;
        return baseFunctionMap.get(baseFunction);
    }

    public TypeInstantiation getTypeInstantiation() {
        Map<FTypeVariable, FType> typeInstantiation = new HashMap<>();
        //noinspection unchecked
        List<FTypeVariable> baseParameters = (List<FTypeVariable>) baseClass.getParametersList();
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
    public boolean canImplicitlyCast() {
        if (!delegates.isEmpty()) //TODO should this be baseClass delegates?
            return true;
        for (int i = 0; i < instantiatedParameters.size(); i++) {
            Variance var = getParameterVariance(i);
            if (var == Variance.Covariant && instantiatedParameters.get(i).canImplicitlyCast())
                return true;
            if (var == Variance.Contravariant)
                return true;
        }
        return false;
    }

    @Override
    public Variance getParameterVariance(FTypeVariable parameter) {
        return baseClass.getParameterVariance(parameter);
    }

    @Override
    public Variance getParameterVariance(int i) {
        return baseClass.getParameterVariance(i);
    }

    @Override
    public FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments {
        return baseClass.getInstantiation(types);
    }

    public boolean isBaked() {
        return baked;
    }

    public void bake() {
        assert !baked;
        GenericBaking.bake(this);
    }

    public void setBaked() {
        baked = true;
    }
}
