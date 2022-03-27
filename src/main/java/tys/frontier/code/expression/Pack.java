package tys.frontier.code.expression;

import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

public class Pack extends FExpression {

    private List<FExpression> expressions;

    public Pack(Position position, List<FExpression> expressions) {
        super(position);
        assert expressions.size() > 1;
        assert expressions.stream().noneMatch(e -> e instanceof Unpack.UnpackedElement);
        this.expressions = expressions;
    }

    public Pack(Position position, List<FExpression> expressions, List<FType> targetTypes) throws IncompatibleTypes {
        this(position, expressions);
        assert expressions.size() == targetTypes.size();

        for (int i = 0; i < this.expressions.size(); i++)
            this.expressions.set(i, expressions.get(i).typeCheck(targetTypes.get(i)));
    }

    @Override
    public FTuple getType() {
        return (FTuple) FTuple.from(Utils.typesFromExpressionList(expressions)); //TODO cache?
    }

    public List<FExpression> getExpressions() {
        return expressions;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterPack(this);
        List<E> expressions = new ArrayList<>(this.expressions.size());
        for (FExpression expression : this.expressions)
            expressions.add(expression.accept(visitor));
        return visitor.exitPack(this, expressions);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitPack(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("pack(");
        Joiners.ON_COMMA.appendTo(sb, expressions);
        return sb.append(')');
    }
}
