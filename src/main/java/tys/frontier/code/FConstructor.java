package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.FFunctionIdentifier;

public class FConstructor extends FFunction {

    public FConstructor(FVisibilityModifier modifier, FClass clazz, ImmutableList<FLocalVariable> params) {
        super(FFunctionIdentifier.CONSTRUCTOR, clazz, modifier, true, clazz, params);
    }

    @Override
    public boolean isConstructor() {
        return true;
    }
}
