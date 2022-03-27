package tys.frontier.code.expression;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.InvalidExpressionBinding;
import tys.frontier.util.Utils;

import java.util.Collection;
import java.util.Optional;

public abstract class UnboundExpression extends FExpression {


    protected UnboundExpression(Position position) {
        super(position);
    }

    public abstract FExpression bind(TypeInstantiation typeInstantiation) throws InvalidExpressionBinding;
    public abstract FExpression bind(FType targetType) throws InvalidExpressionBinding;

    public abstract Collection<FTypeVariable> getIn();
    public abstract Optional<FTypeVariable> getOut();

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return Utils.cantHappen();
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return Utils.cantHappen();
    }
}
