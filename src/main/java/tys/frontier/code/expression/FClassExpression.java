package tys.frontier.code.expression;

import tys.frontier.code.FClass;
import tys.frontier.code.FType;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FClassExpression implements FExpression {

    private FType fClass;

    public FClassExpression(FType fClass) {
        this.fClass = fClass;
    }

    public FType getfClass() {
        return fClass;
    }

    @Override
    public FClass getType() {
        return FTypeType.INSTANCE;
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
