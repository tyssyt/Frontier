package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.statement.FBlock;

public class FInstantiatedFunction extends FFunction {

    private FFunction base;
    private TypeInstantiation typeInstantiation;
    private boolean baked = false;

    private FInstantiatedFunction(FFunction base, FClass memberOf,  TypeInstantiation typeInstantiation) {
        super(base.getIdentifier(), memberOf, base.getVisibility(), base.isNative(), typeInstantiation.getType(base.getType()), createParams(base.getParams(), typeInstantiation));
        this.base = base;
        this.typeInstantiation = typeInstantiation;
    }

    private static ImmutableList<FParameter> createParams(ImmutableList<FParameter> original, TypeInstantiation typeInstantiation) {
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        for (FParameter p : original) {
            res.add(FParameter.create(p.getIdentifier(), typeInstantiation.getType(p.getType()), p.hasDefaultValue()));
        }
        return res.build();
    }

    static FInstantiatedFunction fromClassInstantiation(FInstantiatedClass _class, FFunction base) {
        assert base.getMemberOf() == _class.getBaseClass();
        assert !(base instanceof FInstantiatedFunction);
        return new FInstantiatedFunction(base, _class, _class.getTypeInstantiation());
    }

    static FInstantiatedFunction fromFunctionInstantiation(FFunction base, TypeInstantiation typeInstantiation) {
        if (base instanceof FInstantiatedFunction) {
            TypeInstantiation baseTypeInstantiation = ((FInstantiatedFunction) base).getTypeInstantiation();
            assert baseTypeInstantiation.disjoint(typeInstantiation);
            typeInstantiation = typeInstantiation.then(baseTypeInstantiation);
        }
        return new FInstantiatedFunction(base, (FClass) base.getMemberOf(), typeInstantiation);
    }

    public FFunction getBase() {
        return base;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
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
}
