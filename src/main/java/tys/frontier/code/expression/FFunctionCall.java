package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;

import java.util.List;

public class FFunctionCall implements FExpression {

    private FFunction function;
    private List<? extends FExpression> params;

    public FFunctionCall(FFunction function, List<? extends FExpression> params) {
        this.function = function;
        this.params = params;
    }

    @Override
    public FClass getType() {
        return function.getType();
    }
}
