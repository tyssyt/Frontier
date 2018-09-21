package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.Optional;

public class FFor extends FLoop {

    private FVarDeclaration declaration; //optional
    private FExpression condition; //optional
    private FExpression increment; //optional

    private FFor(int nestedDepth, FLoopIdentifier identifier, FVarDeclaration declaration, FExpression condition, FExpression increment, FBlock body) throws IncompatibleTypes {
        super(nestedDepth, identifier, body);
        this.declaration = declaration;
        this.condition = condition;
        this.increment = increment;
        checkTypes();
    }

    public static FFor create(int nestedDepth, FLoopIdentifier identifier, FVarDeclaration declaration, FExpression condition, FExpression increment, FBlock body) throws IncompatibleTypes {
        return new FFor(nestedDepth, identifier, declaration, condition, increment, body);
    }
    public static FFor createTrusted(int nestedDepth, FLoopIdentifier identifier, FVarDeclaration declaration, FExpression condition, FExpression increment, FBlock body) {
        try {
            return create(nestedDepth, identifier, declaration, condition, increment, body);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public Optional<FVarDeclaration> getDeclaration() {
        return Optional.ofNullable(declaration);
    }

    public Optional<FExpression> getCondition() {
        return Optional.ofNullable(condition);
    }

    public Optional<FExpression> getIncrement() {
        return Optional.ofNullable(increment);
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterFor(this);
        Optional<S> decl = getDeclaration().map(declaration -> declaration.accept(visitor));
        Optional<E> cond = getCondition().map(condition -> condition.accept(visitor));
        Optional<E> incr = getIncrement().map(increment -> increment.accept(visitor));
        return visitor.exitFor(this, decl, cond, incr, getBody().accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitFor(this);
    }

    private void checkTypes() throws IncompatibleTypes {
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
        return getBody().toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
