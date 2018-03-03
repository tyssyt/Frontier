package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

import java.util.Optional;

public class FIf implements FStatement, NeedsTypeCheck {

    private FExpression condition;
    private FStatement then;
    private FStatement elze; //Optional

    public FIf(FExpression condition, FStatement then, FStatement elze) {
        this.condition = condition;
        this.then = then;
        this.elze = elze;
    }

    public FExpression getCondition() {
        return condition;
    }

    public FStatement getThen() {
        return then;
    }

    public Optional<FStatement> getElse() {
        return Optional.ofNullable(elze);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterIf(this);
        return visitor.exitIf(this, condition.accept(visitor), then.accept(visitor), getElse().map(statement -> statement.accept(visitor)));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitIf(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("if (");
        condition.toString(sb).append(") then ");
        then.toString(sb);
        getElse().ifPresent(elze -> elze.toString(sb.append(" else ")));
        return sb;
    }
    @Override
    public String toString() {
        return tS();
    }
}
