package tys.frontier.code.statement.loop;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FForEach extends FLoop implements NeedsTypeCheck {

    private FLocalVariable iterator;
    private FExpression container;

    public FForEach(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FStatement body) {
        super(nestedDepth, identifier, body);
        this.iterator = iterator;
        this.container = container;
    }

    public FLocalVariable getIterator() {
        return iterator;
    }

    public FExpression getContainer() {
        return container;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterForEach(this);
        return visitor.exitForEach(this, container.accept(visitor), getBody().accept(visitor));
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitForEach(this);
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {
        if (FArray.getArrayFrom(iterator.getType(), 1) != container.getType())
            throw new IncompatibleTypes(container.getType(), FArray.getArrayFrom(iterator.getType(), 1));
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("for (").append(iterator).append(" : ");
        container.toString(sb).append(") ");
        return getBody().toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
