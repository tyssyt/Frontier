package tys.frontier.code;

import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.passes.GenericBaking;

public class FInstantiatedClass extends FClass {

    private boolean baked = false;
    private FClass baseClass;
    private TypeInstantiation typeInstantiation;

    FInstantiatedClass(FClass baseClass, TypeInstantiation typeInstantiation) {
        super(new FInstantiatedClassIdentifier(baseClass.getIdentifier(), typeInstantiation), baseClass.getVisibility());
        assert !(baseClass instanceof FInstantiatedClass);
        this.baseClass = baseClass;
        this.typeInstantiation = typeInstantiation;
    }

    void prepare() {
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
            FInstantiatedFunction instantiatedFunction = FInstantiatedFunction.fromClassInstantiation(this, baseFunction);
            this.addFunctionTrusted(instantiatedFunction);
        }

        //constructor
        setConstructorVisibility(baseClass.getConstructorVisibility());
        generateConstructor();
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
    }


    @Override
    public long concreteness() {
        long res = Long.MAX_VALUE;
        for (FTypeVariable param : baseClass.getParametersList()) {
            res = Long.min(res, typeInstantiation.getType(param).concreteness());
        }
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }

    @Override
    public boolean canImplicitlyCast() {
        if (!delegates.isEmpty()) //TODO should this be baseClass delegates?
            return true;
        for (FTypeVariable param : baseClass.getParametersList()) {
            Variance var = baseClass.getParameterVariance(param);
            if (var == Variance.Covariant) {
                FType inst = typeInstantiation.getType(param);
                if (inst.canImplicitlyCast())
                    return true;
            } else if (var == Variance.Contravariant) {
                return true;
            }
        }
        return false;
    }

    @Override
    public FClass getInstantiation(TypeInstantiation typeInstantiation) { //TODO this could be far more optimized...
        if (typeInstantiation.isEmpty())
            return this;
        TypeInstantiation combined = this.typeInstantiation.then(typeInstantiation);
        return baseClass.getInstantiation(combined);
    }

    public boolean isBaked() {
        return baked;
    }

    public void bake() {
        assert !baked && typeInstantiation.fits(baseClass);
        GenericBaking.bake(this);
    }

    public void setBaked() {
        baked = true;
    }
}
