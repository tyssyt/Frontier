package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.Operator.FOperator;

public class FConstructor extends FFunction {

    public FConstructor(FVisibilityModifier modifier, FClass clazz, ImmutableList<FLocalVariable> params) {
        super(FOperator.CONSTRUCTOR, clazz, modifier, true, clazz, params);
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.CONSTRUCTOR;
    }

    @Override
    public boolean isConstructor() {
        return true;
    }
}
