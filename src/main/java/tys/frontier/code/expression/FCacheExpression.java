package tys.frontier.code.expression;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVariable;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FCacheExpression implements FExpression {

    private FLocalVariable variable;
    private FExpression expression;

    private FCacheExpression(FLocalVariable variable, FExpression expression) {
        assert variable.getType() == expression.getType();
        this.variable = variable;
        this.expression = expression;
    }

    public static FCacheExpression create(String name, FExpression expression) {
        return new FCacheExpression(new FLocalVariable(new FIdentifier(name), expression.getType()), expression);
    }

    public FLocalVariable getVariable() {
        return variable;
    }

    public FExpression getExpression() {
        return expression;
    }

    @Override
    public FType getType() {
        return variable.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterCache(this);
        return visitor.exitCache(this, expression.accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitCache(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(variable).append(":=");
        return expression.toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
