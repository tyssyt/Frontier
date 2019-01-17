package tys.frontier.code;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Utils;

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
    public FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments {
        return Utils.NYI("specifying within an instantiated class");
    }

    @Override
    public FClass getInstantiation(TypeInstantiation typeInstantiation) {
        return Utils.NYI("specifying within an instantiated class");
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
