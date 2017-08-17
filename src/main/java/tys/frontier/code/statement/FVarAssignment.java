package tys.frontier.code.statement;

import tys.frontier.code.FVariable;
import tys.frontier.code.expression.FExpression;

public class FVarAssignment implements FStatement {

    FVariable variable;
    Operator operator;
    FExpression value;
    enum Operator { //TODO type restrictions of operators
        ASSIGN("="),
        ADD_ASSIGN("+="),
        SUB_ASSIGN("-="),
        MUL_ASSIGN("*="),
        DIV_ASSIGN("/="),
        AND_ASSIGN("&="),
        OR_ASSIGN("|="),
        XOR_ASSIGN("^="),
        MOD_ASSIGN("%=");

        public final String stringRepresentation;

        Operator(String s) {
            stringRepresentation = s;
        }
        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}
