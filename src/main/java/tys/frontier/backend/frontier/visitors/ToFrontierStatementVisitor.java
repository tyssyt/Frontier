package tys.frontier.backend.frontier.visitors;

import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.style.Indenter;
import tys.frontier.style.Style;

import java.util.List;
import java.util.Optional;

public class ToFrontierStatementVisitor extends StatementVisitor.Default<StringBuilder, StringBuilder> {
    //TODO indention
    //TODO most other things style

    private Style style;
    private Indenter indenter;

    public ToFrontierStatementVisitor(Style style) {
        super(new ToFrontierExpressionVisitor(style));
        indenter = style.createIndenter();
    }

    @Override
    public StringBuilder visitEmpty(FEmptyStatement statement) {
        return new StringBuilder().append(';');
    }

    @Override
    public StringBuilder visitBreak(FBreak fBreak) {
        return new StringBuilder(style.getKeyword(FrontierParser.BREAK)).append(';');
    }

    @Override
    public StringBuilder visitContinue(FContinue fContinue) {
        return new StringBuilder(style.getKeyword(FrontierParser.CONTINUE)).append(';');
    }

    @Override
    public StringBuilder exitExpression(FExpressionStatement statement, StringBuilder expression) {
        return expression.append(';');
    }

    @Override
    public StringBuilder exitReturn(FReturn fReturn, Optional<StringBuilder> value) {
        StringBuilder sb = new StringBuilder(style.getKeyword(FrontierParser.RETURN));
        value.ifPresent(v -> sb.append(' ').append(v));
        return sb.append(';');
    }

    @Override
    public StringBuilder exitVarDeclaration(FVarDeclaration declaration, Optional<StringBuilder> value) {
        //TODO deal with predefined types
        StringBuilder sb = new StringBuilder(declaration.getVar().getType().getIdentifier().name)
                .append(' ').append(declaration.getVar().getIdentifier().name);
        value.ifPresent(v -> sb.append(" = ").append(v));
        return sb.append(';');
    }

    @Override
    public StringBuilder exitVarAssignment(FVarAssignment assignment, Optional<StringBuilder> object, StringBuilder value) {
        //TODO exitVarAssignemnt should reuse fieldAccess if it is one, and variable otherwise
        //yes this is a one liner, because i can and I don't care about readability!
        return object.map(o ->
            o.append('.').append(((FFieldAccess) assignment.getVariableExpression()).getField().getIdentifier().name)
        ).orElse(new StringBuilder(((FLocalVariableExpression) assignment.getVariableExpression()).getVariable().getIdentifier().name))
        .append(' ').append(assignment.getOperator().stringRepresentation).append(' ').append(value).append(';');
    }

    @Override
    public StringBuilder exitBlock(FBlock block, List<StringBuilder> statements) {
        StringBuilder sb = new StringBuilder().append('{');
        indenter.increase();
        for (StringBuilder s : statements) {
            indenter.appendNewLine(sb).append(s);
        }
        indenter.decrease();
        indenter.appendNewLine(sb).append('}');
        return sb;
    }

    //TODO yeah bottom up does not work, because we might want to increase the indention of a block...
    //TODO außer replace \n -> \n + indent

    @Override
    public StringBuilder exitIf(FIf fIf, StringBuilder cond, StringBuilder then, Optional<StringBuilder> elze) {
        StringBuilder sb = new StringBuilder(style.getKeyword(FrontierParser.IF));
        if (style.getOptions().spaceAfterConditional)
            sb.append(' ');
        sb.append('(').append(cond).append(')');
        return null; //TODO
    }

    @Override
    public StringBuilder exitWhile(FWhile fWhile, StringBuilder cond, StringBuilder body) {
        return null;
    }

    @Override
    public StringBuilder exitFor(FFor fFor, Optional<StringBuilder> declaration, Optional<StringBuilder> condition, Optional<StringBuilder> increment, StringBuilder body) {
        return null;
    }

    @Override
    public StringBuilder exitForEach(FForEach forEach, StringBuilder container, StringBuilder body) {
        return null;
    }
}
