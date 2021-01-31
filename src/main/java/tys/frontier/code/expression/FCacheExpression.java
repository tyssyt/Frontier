package tys.frontier.code.expression;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;

public class FCacheExpression extends FExpression {

    private FLocalVariable variable;
    private FExpression expression;

    private FCacheExpression(Position position, FLocalVariable variable, FExpression expression) {
        super(position);
        assert variable.getType() == expression.getType();
        this.variable = variable;
        this.expression = expression;
    }

    public static FCacheExpression create(String name, FExpression expression) {
        return new FCacheExpression(expression.getPosition(), new FLocalVariable(expression.getPosition(), new FIdentifier(name), expression.getType()), expression);
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
}
