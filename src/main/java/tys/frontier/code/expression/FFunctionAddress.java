package tys.frontier.code.expression;

import tys.frontier.code.FFunction;
import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FFunctionAddress implements FExpression {

    private FFunction function;

    public FFunctionAddress(FFunction function) {
        this.function = function;
    }

    @Override
    public FType getType() {
        return FFunctionType.from(function);
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return null; //TODO
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return null; //TODO
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return function.getSignature().toString(sb).append('*');
    }

    @Override
    public String toString() {
        return tS();
    }
}
