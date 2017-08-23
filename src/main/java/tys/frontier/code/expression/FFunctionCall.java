package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.visitor.ExpressionVisitor;

import java.util.List;

public class FFunctionCall implements FExpression {

    private FFunction function;
    private List<? extends FExpression> params;

    public FFunctionCall(FFunction function, List<? extends FExpression> params) {
        this.function = function;
        this.params = params;
    }

    public FFunction getFunction() {
        return function;
    }

    public List<? extends FExpression> getParams() {
        return params;
    }

    @Override
    public FClass getType() {
        return function.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.enterFunctionCall(this);
    }
}
