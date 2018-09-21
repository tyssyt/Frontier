package tys.frontier.code.statement.loop;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FForEach extends FLoop {

    private FLocalVariable iterator;
    private FExpression container;

    private FForEach(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FBlock body) throws IncompatibleTypes {
        super(nestedDepth, identifier, body);
        this.iterator = iterator;
        this.container = container;
        checkTypes();
    }

    public static FForEach create(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FBlock body) throws IncompatibleTypes {
        return new FForEach(nestedDepth, identifier, iterator, container, body);
    }
    public static FForEach createTrusted(int nestedDepth, FLoopIdentifier identifier, FLocalVariable iterator, FExpression container, FBlock body) {
        try {
            return create(nestedDepth, identifier, iterator, container, body);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
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

    private void checkTypes() throws IncompatibleTypes {
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
