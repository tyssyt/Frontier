package tys.frontier.code.expression.cast;

import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Variance;

public class LiteralCast extends ImplicitTypeCast {

    public LiteralCast(FType base, FType target, Variance variance) {
        super(base, target, variance);
    }

    @Override
    public long getCost() {
        return 0;
    }

    @Override
    public boolean isNoOpCast() {
        return true;
    }
}
