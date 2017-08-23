package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

import java.util.Optional;

public class FFor implements FLoop, NeedsTypeCheck {

    private FLoopIdentifier identifier;
    private FVarDeclaration declaration; //optional
    private FExpression condition; //optional
    private FExpression increment; //optional
    private FStatement body;

    public FFor(FLoopIdentifier identifier, FVarDeclaration declaration, FExpression condition, FExpression increment, FStatement body) {
        this.identifier = identifier;
        this.declaration = declaration;
        this.condition = condition;
        this.increment = increment;
        this.body = body;
    }

    @Override
    public FLoopIdentifier getIdentifier() {
        return identifier;
    }

    public Optional<FVarDeclaration> getDeclaration() {
        return declaration == null ? Optional.empty() : Optional.of(declaration);
    }

    public Optional<FExpression> getCondition() {
        return condition == null ? Optional.empty() : Optional.of(condition);
    }

    public Optional<FExpression> getIncrement() {
        return increment == null ? Optional.empty() : Optional.of(increment);
    }

    public FStatement getBody() {
        return body;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterFor(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition != null && condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("for (");
        getDeclaration().ifPresent(d -> d.toString(sb));
        sb.append("; ");
        getCondition().ifPresent(c -> c.toString(sb));
        sb.append("; ");
        getIncrement().ifPresent(i -> i.toString(sb));
        sb.append(") ");
        return body.toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
