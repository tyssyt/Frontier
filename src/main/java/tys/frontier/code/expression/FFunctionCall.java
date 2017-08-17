package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;

import java.util.List;

public class FFunctionCall implements FExpression {

    FFunction function;
    List<FExpression> params;

    @Override
    public FClass getType() {
        return function.getType();
    }
}
