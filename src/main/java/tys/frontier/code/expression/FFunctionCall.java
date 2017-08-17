package tys.frontier.code.expression;

import tys.frontier.code.FFunction;
import tys.frontier.code.type.FType;

import java.util.List;

public class FFunctionCall implements FExpression {

    FFunction function;
    List<FExpression> params;

    @Override
    public FType getType() {
        return function.getType();
    }
}
