package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.visitor.ExpressionVisitor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class FFunctionCall implements FExpression {
    private FExpression object;
    private FFunction function;
    private List<? extends FExpression> params;

    public FFunctionCall(FExpression object, FFunction function, List<? extends FExpression> params) {
        this.object = object;
        this.function = function;
        this.params = params;
    }

    public FFunctionCall(FFunction function, List<? extends FExpression> params) {
        assert (function.isStatic());
        this.function = function;
        this.params = params;
    }

    public FExpression getObject() {
        return object;
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
        visitor.enterFunctionCall(this);
        E object = visitor.visit(this.object);
        List<E> params = new ArrayList<>(this.params.size());
        for (FExpression expression : this.params)
            params.add(visitor.visit(expression));
        return visitor.exitFunctionCall(this, object, params);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        if (object == null)
            sb.append(function.getClazz().getIdentifier());
        else
            object.toString(sb);
        sb.append('.').append(function.getIdentifier()).append('(');
        Iterator<? extends FExpression> it = params.iterator();
        if (it.hasNext()) {
            it.next().toString(sb);
            while (it.hasNext())
                it.next().toString(sb.append(", "));
        }
        return sb.append(')');
    }
    @Override
    public String toString() {
        return tS();
    }
}
