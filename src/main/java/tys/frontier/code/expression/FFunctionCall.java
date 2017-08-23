package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FFunction;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.StaticAccessToInstanceFunction;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

public class FFunctionCall implements FExpression {
    private FExpression object;
    private FFunction function;
    private List<? extends FExpression> params;

    public FFunctionCall(FExpression object, FFunction function, List<? extends FExpression> params) {
        this.object = object;
        this.function = function;
        this.params = params;
    }

    public FFunctionCall(FFunction function, List<? extends FExpression> params) throws StaticAccessToInstanceFunction {
        if (!function.isStatic())
            throw new StaticAccessToInstanceFunction(function);
        this.function = function;
        this.params = params;
    }

    public Optional<FExpression> getObject() {
        return object==null ? Optional.empty() : Optional.of(object);
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

    @Override
    public StringBuilder toString(StringBuilder sb) {
        object.toString(sb).append('.').append(function.getIdentifier()).append('(');
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
