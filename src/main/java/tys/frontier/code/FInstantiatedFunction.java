package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FInstantiatedFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.passes.GenericBaking;
import tys.frontier.util.Utils;

import java.util.Collections;
import java.util.Map;

import static tys.frontier.code.FInstantiatedFunction.InstantiationType.*;

public class FInstantiatedFunction extends FFunction {

    public enum InstantiationType {
        CLASS_INSTANTIATION,
        FUNCTION_INSTANTIATION,
        INSTANTIABLE_COPY
    }

    private FFunction base;
    private TypeInstantiation typeInstantiation;
    private InstantiationType instantiationType;
    private boolean baked = false;

    private FInstantiatedFunction(FFunction base, FClass memberOf,  TypeInstantiation typeInstantiation, InstantiationType instantiationType, Map<FTypeIdentifier, FTypeVariable> parameters) {
        super( instantiationType == CLASS_INSTANTIATION ? base.getIdentifier() : new FInstantiatedFunctionIdentifier(base.getIdentifier(),
               typeInstantiation), memberOf, base.getVisibility(), base.isNative(),
               typeInstantiation.getType(base.getType()), createParams(base.getParams(), typeInstantiation), parameters);
        assert !(base instanceof FInstantiatedFunction);
        this.base = base;
        this.typeInstantiation = typeInstantiation;
        this.instantiationType = instantiationType;
    }

    private static ImmutableList<FParameter> createParams(ImmutableList<FParameter> original, TypeInstantiation typeInstantiation) {
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        for (FParameter p : original) {
            res.add(FParameter.create(p.getIdentifier(), typeInstantiation.getType(p.getType()), p.hasDefaultValue()));
        }
        return res.build();
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        switch (instantiationType) {
            case CLASS_INSTANTIATION:
                return super.getInstantiation(typeInstantiation); //most of the logic for this case is in fromFunctionInstantiation
            case FUNCTION_INSTANTIATION:
                return Utils.cantHappen();
            case INSTANTIABLE_COPY:
                assert typeInstantiation.disjoint(this.typeInstantiation);
                typeInstantiation = typeInstantiation.then(this.typeInstantiation);
                return base.getInstantiation(typeInstantiation);
            default:
                return Utils.cantHappen();
        }
    }

    static FInstantiatedFunction fromClassInstantiation(FInstantiatedClass _class, FFunction base) {
        assert base.getMemberOf() == _class.getBaseClass();
        assert !(base instanceof FInstantiatedFunction);
        return new FInstantiatedFunction(base, _class, _class.getTypeInstantiation(), CLASS_INSTANTIATION, Collections.emptyMap());
    }

    static FInstantiatedFunction fromFunctionInstantiation(FFunction base, TypeInstantiation typeInstantiation, Map<FTypeIdentifier, FTypeVariable> newParams) {
        if (base instanceof FInstantiatedFunction) {
            assert ((FInstantiatedFunction) base).instantiationType == CLASS_INSTANTIATION;
            TypeInstantiation baseTypeInstantiation = ((FInstantiatedFunction) base).getTypeInstantiation();
            assert baseTypeInstantiation.disjoint(typeInstantiation);
            typeInstantiation = typeInstantiation.then(baseTypeInstantiation);
            base = ((FInstantiatedFunction) base).getBase();
        }
        return new FInstantiatedFunction(base, (FClass) base.getMemberOf(), typeInstantiation, FUNCTION_INSTANTIATION, newParams);
    }

    static FInstantiatedFunction instantiableCopy(FFunction base, TypeInstantiation typeInstantiation, Map<FTypeIdentifier, FTypeVariable> newParams) {
        assert !(base instanceof FInstantiatedFunction);
        return new FInstantiatedFunction(base, (FClass) base.getMemberOf(), typeInstantiation, INSTANTIABLE_COPY, newParams);
    }

    public FFunction getBase() {
        return base;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
    }

    public InstantiationType getInstantiationType() {
        return instantiationType;
    }

    public boolean isBaked() {
        return baked;
    }

    @Override
    public boolean addCall(FFunctionCall call) {
        base.addCall(call);
        return super.addCall(call);
    }

    @Override
    public void setBody(FBlock body) {
        super.setBody(body);
        baked = true;
    }

    public void bake() {
        if (baked)
            return;
        GenericBaking.bake(this);
    }

    public void setBaked() {
        baked = true;
    }
}
