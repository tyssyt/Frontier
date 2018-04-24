package tys.frontier.code.expression;

import tys.frontier.code.FClass;

public abstract class FExplicitCast extends FCast {

    public FExplicitCast(FClass type, FExpression castedExpression) {
        super(type, castedExpression);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append('(').append(getType().getIdentifier().name).append(')');
        return getCastedExpression().toString(sb);
    }
}
