package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.identifier.FFunctionIdentifier;

public abstract class FOperator extends FFunction {

    public FOperator(FFunctionIdentifier identifier, FClass fClass, boolean statik, FClass returnType, ImmutableList<FParameter> params) {
        super(identifier, fClass, fClass.getVisibility(), false, statik, returnType, params);
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.OPERATOR;
    }
}