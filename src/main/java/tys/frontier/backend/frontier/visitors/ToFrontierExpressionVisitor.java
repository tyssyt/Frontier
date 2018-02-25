package tys.frontier.backend.frontier.visitors;

import tys.frontier.code.expression.*;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.style.Style;

import java.util.Iterator;
import java.util.List;

public class ToFrontierExpressionVisitor extends ExpressionVisitor.Default<StringBuilder> {
    //TODO everything stylefile

    private Style style;

    ToFrontierExpressionVisitor(Style style) {
        this.style = style;
    }

    @Override
    public StringBuilder visitLiteral(FLiteralExpression expression) {
        return new StringBuilder(expression.getLiteral().getOriginalString());
    }

    @Override
    public StringBuilder visitVariable(FLocalVariableExpression expression) {
        //TODO resolve name clashes with identifiers
        return new StringBuilder(expression.getVariable().getIdentifier().name);
    }

    @Override
    public StringBuilder exitArrayAccess(FArrayAccess arrayAccess, StringBuilder array, StringBuilder index) {
        return array.append('[').append(index).append(']');
    }

    @Override
    public StringBuilder exitBrackets(FBracketsExpression brackets, StringBuilder inner) {
        return inner.insert(0, '(').append(')');
    }

    @Override
    public StringBuilder exitFunctionCall(FFunctionCall functionCall, StringBuilder object, List<StringBuilder> params) {
        //TODO operators
        object.append('.').append(functionCall.getFunction().getIdentifier().name);
        if (style.getOptions().spaceAfterMethodCall)
            object.append(' ');
        object.append('(');
        Iterator<StringBuilder> it = params.iterator();
        if (it.hasNext())
            object.append(it.next());
        while (it.hasNext())
            object.append(',').append(' ').append(it.next());
        return object.append(')');
    }

    @Override
    public StringBuilder exitFieldAccess(FFieldAccess fieldAccess, StringBuilder object) {
        return object.append('.').append(fieldAccess.getField().getIdentifier().name);
    }
}
