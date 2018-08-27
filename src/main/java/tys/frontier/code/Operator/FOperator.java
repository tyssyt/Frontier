package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.identifier.FFunctionIdentifier;

public abstract class FOperator extends FFunction {

    public FOperator(FFunctionIdentifier identifier, FType fType, boolean statik, FType returnType, ImmutableList<FParameter> params) {
        super(identifier, fType, fType.getVisibility(), statik, returnType, params);
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.OPERATOR;
    }
}