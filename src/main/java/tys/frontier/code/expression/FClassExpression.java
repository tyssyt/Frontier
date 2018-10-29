package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.predefinedClasses.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FClassExpression implements FExpression {

    private FClass fClass;

    public FClassExpression(FClass fClass) {
        this.fClass = fClass;
    }

    public FClass getfClass() {
        return fClass;
    }

    @Override
    public FClass getType() {
        return FType.INSTANCE;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitClassExpr(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitClassExpr(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(fClass.getIdentifier().name);
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
