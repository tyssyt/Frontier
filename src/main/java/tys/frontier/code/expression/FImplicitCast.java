package tys.frontier.code.expression;

import tys.frontier.code.FClass;

public abstract class FImplicitCast extends FCast {

    public FImplicitCast(FClass type, FExpression castedExpression) {
        super(type, castedExpression);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return getCastedExpression().toString(sb);
    }
}
