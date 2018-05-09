package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.identifier.FFunctionIdentifier;

public abstract class FOperator extends FFunction {

    public static final FFunctionIdentifier CONSTRUCTOR = new FFunctionIdentifier("!new"); //take a name that would be invalid for the parser to avoid clashing with user defined functions


    public FOperator(FFunctionIdentifier identifier, FClass clazz, boolean statik, FClass returnType, ImmutableList<FLocalVariable> params) {
        super(identifier, clazz, clazz.getVisibility(), statik, returnType, params);
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.OPERATOR;
    }
}