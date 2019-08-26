package tys.frontier.code.expression;

import tys.frontier.code.function.FFunction;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.util.Utils;

public class FFunctionAddress implements FExpression {

    private FFunction function;

    public FFunctionAddress(FFunction function) {
        this.function = function;
    }

    public FFunction getFunction() {
        return function;
    }

    @Override
    public FType getType() {
        return FFunctionType.from(function);
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitFunctionAddress(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitFunctionAddress(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(function.getIdentifier()).append('(');
        return Utils.joinIdentifiers(sb, function.getParams(), ",").append(")*");
    }

    @Override
    public String toString() {
        return tS();
    }
}
