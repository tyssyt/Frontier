package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FWhile extends FLoop {

    private FExpression condition;

    private FWhile(int nestedDepth, FLoopIdentifier identifier, FExpression condition, FBlock body) throws IncompatibleTypes {
        super(nestedDepth, identifier, body);
        this.condition = condition;
        checkTypes();
    }

    public static FWhile create(int nestedDepth, FLoopIdentifier identifier, FExpression condition, FBlock body) throws IncompatibleTypes {
        return new FWhile(nestedDepth, identifier, condition, body);
    }
    public static FWhile createTrusted(int nestedDepth, FLoopIdentifier identifier, FExpression condition, FBlock body) {
        try {
            return create(nestedDepth, identifier, condition, body);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
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

    private void checkTypes() throws IncompatibleTypes {
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
