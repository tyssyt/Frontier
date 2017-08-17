package tys.frontier.code.expression;

import tys.frontier.code.type.FType;

public class FBinaryOp implements FExpression {

    private FExpression first;
    private FExpression second;
    private Operator operator;
    public FBinaryOp(FExpression first, FExpression second, Operator operator) {
        this.first = first;
        this.second = second;
        this.operator = operator;
    }

    @Override
    public FType getType() {
        return null; //TODO
    }

    public enum Operator { //TODO type restrictions of operators
        EQ("=="),
        NE("!="),
        AND("&&"),
        OR("||"),
        XOR("^"),
        LE("<="),
        GE("<="),
        LT("<"),
        GT(">"),
        MUL("*"),
        DIV("/"),
        MOD("%"),
        ADD("+"),
        SUB("-");

        public final String stringRepresentation;

        Operator(String s) {
            stringRepresentation = s;
        }

        public static Operator fromString (String string) {
            for (Operator op : values()) {
                if (op.stringRepresentation.equals(string))
                    return op;
            }
            return null;
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}
