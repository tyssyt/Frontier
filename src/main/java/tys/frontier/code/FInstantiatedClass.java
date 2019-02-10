package tys.frontier.code;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.passes.GenericBaking;

import java.util.List;

public class FInstantiatedClass extends FClass {

    private boolean baked = false;
    private FClass baseClass;
    private TypeInstantiation typeInstantiation;
    private BiMap<FFunction, FFunction> shimMap = HashBiMap.create();

    FInstantiatedClass(FClass baseClass, TypeInstantiation typeInstantiation) {
        super(new FInstantiatedClassIdentifier(baseClass.getIdentifier(), typeInstantiation), baseClass.getVisibility());
        assert !(baseClass instanceof FInstantiatedClass);
        this.baseClass = baseClass;
        this.typeInstantiation = typeInstantiation;
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    public FFunction getInstantiatedFunction(FFunction function) {
        return shimMap.computeIfAbsent(function, f -> FInstantiatedFunction.fromClassInstantiation(this, f));
    }

    public FFunction getOriginalFunction(FFunction function) {
        return shimMap.inverse().get(function);
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
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
        baked = true;
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        if (baked) {
            return super.resolveFunction(identifier, arguments, typeInstantiation);
        } else {
            FFunction base = baseClass.resolveFunction(identifier, arguments, typeInstantiation.then(this.typeInstantiation));
            return getInstantiatedFunction(base);
        }
    }
}
