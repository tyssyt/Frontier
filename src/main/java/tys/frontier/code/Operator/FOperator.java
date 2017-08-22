package tys.frontier.code.Operator;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.List;

public abstract class FOperator extends FFunction {

    public FOperator(FFunctionIdentifier identifier, FClass clazz, FClass returnType, List<FLocalVariable> params) {
        super(identifier, clazz, FVisibilityModifier.PUBLIC, true, returnType, params);
    }
}