package tys.frontier.code;

import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.List;

public class FConstructor extends FFunction {

    public FConstructor(FVisibilityModifier modifier, FClass clazz, List<FVariable> params) {
        super(FFunctionIdentifier.CONSTRUCTOR, clazz, modifier, true, clazz.getType(), params);
    }

    @Override
    public boolean isConstructor() {
        return true;
    }
}
