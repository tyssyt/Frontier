package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

public class FWhile implements FLoop, NeedsTypeCheck {

    private FLoopIdentifier identifier;
    private FExpression condition;
    private FStatement body;

    public FWhile(FLoopIdentifier identifier, FExpression condition, FStatement body) {
        this.identifier = identifier;
        this.condition = condition;
        this.body = body;
    }

    @Override
    public FLoopIdentifier getIdentifier() {
        return identifier;
    }

    public FExpression getCondition() {
        return condition;
    }

    public FStatement getBody() {
        return body;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterWhile(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }
}
