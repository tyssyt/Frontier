package tys.frontier.code.visitor;

import tys.frontier.code.expression.*;

import java.util.ArrayList;
import java.util.List;

public interface ExpressionVisitor<Expression>  {

    //Top down
    Expression enterExpression(FExpression expression);

    Expression enterArrayAccess(FArrayAccess arrayAccess);

    Expression enterBrackets(FBracketsExpression brackets);

    Expression enterFunctionCall(FFunctionCall functionCall);

    //Bottom Up
    Expression exitArrayAccess(FArrayAccess arrayAccess, Expression array, Expression index);

    Expression exitBrackets(FBracketsExpression brackets, Expression inner);

    Expression exitFunctionCall(FFunctionCall functionCall, List<Expression> params);

    //Leafs
    Expression visitFieldAccess(FFieldAccess fieldAccess);

    Expression visitLiteral(FLiteralExpression expression);

    Expression visitVariable(FLocalVariableExpression expression);

    abstract class Default<Expression> implements ExpressionVisitor<Expression> {
        public Expression getDefault() {
            return null;
        }

        //Top Down
        @Override
        public Expression enterExpression(FExpression expression) {
            return expression.accept(this);
        }

        @Override
        public Expression enterArrayAccess(FArrayAccess arrayAccess) {
            return exitArrayAccess(arrayAccess, enterExpression(arrayAccess.getArray()), enterExpression(arrayAccess.getIndex()));
        }

        @Override
        public Expression enterBrackets(FBracketsExpression brackets) {
            return exitBrackets(brackets, enterExpression(brackets.getInner()));
        }

        @Override
        public Expression enterFunctionCall(FFunctionCall functionCall) {
            List<Expression> params = new ArrayList<>(functionCall.getParams().size());
            for (FExpression expression : functionCall.getParams())
                params.add(enterExpression(expression));
            return exitFunctionCall(functionCall, params);
        }

        //Bottom Up
        @Override
        public Expression exitArrayAccess(FArrayAccess arrayAccess, Expression array, Expression index) {
            return getDefault();
        }

        @Override
        public Expression exitBrackets(FBracketsExpression brackets, Expression inner) {
            return getDefault();
        }

        @Override
        public Expression exitFunctionCall(FFunctionCall functionCall, List<Expression> params) {
            return getDefault();
        }

        //leafs
        @Override
        public Expression visitFieldAccess(FFieldAccess fieldAccess) {
            return getDefault();
        }

        @Override
        public Expression visitLiteral(FLiteralExpression expression) {
            return getDefault();
        }

        @Override
        public Expression visitVariable(FLocalVariableExpression expression) {
            return getDefault();
        }
    }
}
