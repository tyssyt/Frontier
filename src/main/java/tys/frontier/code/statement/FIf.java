package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

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
        return elze == null ? Optional.empty() : Optional.of(elze);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (condition.getType() != FBool.INSTANCE)
            throw new IncompatibleTypes(FBool.INSTANCE, condition.getType());
    }
}
