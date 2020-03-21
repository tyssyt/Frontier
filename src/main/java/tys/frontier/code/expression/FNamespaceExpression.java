package tys.frontier.code.expression;

import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.type.FClass;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

public class FNamespaceExpression implements FExpression {

    private Namespace namespace;

    public FNamespaceExpression(Namespace namespace) {
        this.namespace = namespace;
    }

    public Namespace getNamespace() {
        return namespace;
    }

    @Override
    public FClass getType() {
        return FTypeType.INSTANCE;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitNamespaceExpression(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitNamespaceExpression(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(namespace.getIdentifier().name);
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
