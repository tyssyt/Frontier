package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

import java.util.Optional;

public class FFor implements FLoop, NeedsTypeCheck {

    private FLoopIdentifier identifier;
    private FVarDeclaration declaration; //optional
    private FExpression condition; //optional
    private FExpression incement; //optional
    private FStatement body;

    public FFor(FLoopIdentifier identifier, FVarDeclaration declaration, FExpression condition, FExpression incement, FStatement body) {
        this.identifier = identifier;
        this.declaration = declaration;
        this.condition = condition;
        this.incement = incement;
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

    public Optional<FExpression> getIncement() {
        return incement == null ? Optional.empty() : Optional.of(incement);
    }

    public FStatement getBody() {
        return body;
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition != null && condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }
}
