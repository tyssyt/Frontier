package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FWhile extends FLoop implements NeedsTypeCheck {

    private FExpression condition;

    public FWhile(int nestedDepth, FLoopIdentifier identifier, FExpression condition, FStatement body) {
        super(nestedDepth, identifier, body);
        this.condition = condition;
    }

    public FExpression getCondition() {
        return condition;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterWhile(this);
        return visitor.exitWhile(this, condition.accept(visitor), getBody().accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitWhile(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("while (");
        condition.toString(sb).append(") ");
        return getBody().toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
