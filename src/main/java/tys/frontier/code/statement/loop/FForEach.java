package tys.frontier.code.statement.loop;

import tys.frontier.code.FVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.semanticAnalysis.NeedsTypeCheck;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FForEach implements FLoop, NeedsTypeCheck {

    private FLoopIdentifier identifier;
    private FVariable iterator;
    private FExpression container;
    private FStatement body;

    public FForEach(FLoopIdentifier identifier, FVariable iterator, FExpression container, FStatement body) {
        this.identifier = identifier;
        this.iterator = iterator;
        this.container = container;
        this.body = body;
    }

    @Override
    public FLoopIdentifier getIdentifier() {
        return identifier;
    }

    public FVariable getIterator() {
        return iterator;
    }

    public FExpression getContainer() {
        return container;
    }

    public FStatement getBody() {
        return body;
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        return visitor.enterForEach(this);
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
        return body.toString(sb);
    }
    @Override
    public String toString() {
        return tS();
    }
}
