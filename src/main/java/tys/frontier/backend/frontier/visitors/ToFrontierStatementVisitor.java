package tys.frontier.backend.frontier.visitors;

import com.google.common.collect.ImmutableList;
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

    ToFrontierStatementVisitor(Style style, Indenter indenter) {
        super(new ToFrontierExpressionVisitor(style));
        this.indenter = indenter;
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
    public StringBuilder exitExpressionStatement(FExpressionStatement statement, StringBuilder expression) {
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

    //statements that have a block
    public StringBuilder appendBlock(StringBuilder sb, List<FStatement> block, boolean withOpeningBracket) {
        if (withOpeningBracket)
            indenter.appendNewLine(sb).append('{');
        indenter.increase();
        for (FStatement statement : block) {
            indenter.appendNewLine(sb).append(enterStatement(statement));
        }
        indenter.decrease();
        indenter.appendNewLine(sb).append('}');
        return sb;
    }

    @Override
    public StringBuilder enterBlock(FBlock block) {
        StringBuilder sb = new StringBuilder();
        appendBlock(sb, block.getStatements(), true);
        return sb;
    }

    public StringBuilder appendControlStructureStatement(StringBuilder sb, FStatement statement, boolean bracketsOnNewLine, boolean noBracketsForSingleStatement) {
        if (!(statement instanceof FBlock) && !noBracketsForSingleStatement)
            statement = new FBlock(ImmutableList.of(statement));

        if (statement instanceof FBlock) {
            if (bracketsOnNewLine)
                indenter.appendNewLine(sb).append('{');
            else
                sb.append(' ').append('{');
            appendBlock(sb, ((FBlock) statement).getStatements(), false);
        } else {
            indenter.increase();
            sb.append(enterStatement(statement));
            indenter.decrease();
        }
        return sb;
    }

    @Override
    public StringBuilder enterIf(FIf fIf) {
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(FrontierParser.IF));
        if (style.getOptions().spaceAfterConditional)
            sb.append(' ');
        sb.append('(').append(enterExpression(fIf.getCondition())).append(')');

        appendControlStructureStatement(sb, fIf.getThen(),
                style.getOptions().bracketsOnNewLineAfterConditional, style.getOptions().noBracketsForSingleStatementConditional);

        fIf.getElse().ifPresent( (FStatement e) -> {
            if (sb.charAt(sb.length()-1) == '}')
                sb.append(' ');
            sb.append(style.getKeyword(FrontierParser.ELSE));
            appendControlStructureStatement(sb, e,
                    style.getOptions().bracketsOnNewLineAfterConditional, style.getOptions().noBracketsForSingleStatementConditional);
        });
        return sb;
    }
    @Override
    public StringBuilder enterWhile(FWhile fWhile) {
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(FrontierParser.IF));
        if (style.getOptions().spaceAfterLoop)
            sb.append(' ');
        sb.append('(').append(enterExpression(fWhile.getCondition())).append(')');


        appendControlStructureStatement(sb, fWhile.getBody(),
                style.getOptions().bracketsOnNewLineAfterLoop, style.getOptions().noBracketsForSingleStatementLoop);
        return sb;
    }
    @Override
    public StringBuilder enterFor(FFor fFor) {
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(FrontierParser.FOR));
        if (style.getOptions().spaceAfterLoop)
            sb.append(' ');

        //TODO this will need a lot of thought for proper linebreaking...
        sb.append('(');
        if (fFor.getDeclaration().isPresent())
            sb.append(enterStatement(fFor.getDeclaration().get())).append(';').append(' ');
        else
            sb.append(';');
        if (fFor.getCondition().isPresent())
            sb.append(enterExpression(fFor.getCondition().get())).append(';').append(' ');
        else
            sb.append(';');
        if (fFor.getIncrement().isPresent())
            sb.append(enterExpression(fFor.getIncrement().get()));
        sb.append(')');

        appendControlStructureStatement(sb, fFor.getBody(),
                style.getOptions().bracketsOnNewLineAfterLoop, style.getOptions().noBracketsForSingleStatementLoop);
        return sb;
    }
    @Override
    public StringBuilder enterForEach(FForEach forEach) {
        StringBuilder sb = new StringBuilder();
        indenter.appendNewLine(sb).append(style.getKeyword(FrontierParser.FOR));
        if (style.getOptions().spaceAfterLoop)
            sb.append(' ');

        //TODO this will need a lot of thought for proper linebreaking...
        //TODO identifier conflicts
        sb.append('(');
        sb.append(forEach.getIterator().getType().getIdentifier().name).append(' ').append(forEach.getIterator().getIdentifier().name);
        sb.append(" : ").append(enterExpression(forEach.getContainer()));
        sb.append(')');

        appendControlStructureStatement(sb, forEach.getBody(),
                style.getOptions().bracketsOnNewLineAfterLoop, style.getOptions().noBracketsForSingleStatementLoop);
        return sb;
    }
}
