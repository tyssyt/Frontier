package tys.frontier.code;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FInstantiatedClassIdentifier;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;

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
        return shimMap.computeIfAbsent(function, this::createShim);
    }

    public FFunction getOriginalFunction(FFunction function) {
        return shimMap.inverse().get(function);
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
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
    public Pair<FFunction, IntIntPair> resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        if (baked) {
            return super.resolveFunction(identifier, arguments, typeInstantiation);
        } else {
            Pair<FFunction, IntIntPair> res = baseClass.resolveFunction(identifier, arguments, typeInstantiation.then(this.typeInstantiation));
            res.a = getInstantiatedFunction(res.a);
            return res;
        }
    }

    private FFunction createShim(FFunction original) {
        FType returnType = typeInstantiation.getType(original.getType());
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FParameter p : original.getParams()) {
            FType pType = typeInstantiation.getType(p.getType());
            params.add(FParameter.create(p.getIdentifier(), pType, p.hasDefaultValue()));
        }
        return new FFunction(original.getIdentifier(), this, original.getVisibility(), false,
                returnType, params.build()) {
            @Override
            public boolean addCall(FFunctionCall call) {
                original.addCall(call);
                return super.addCall(call);
            }
        };
    }
}
