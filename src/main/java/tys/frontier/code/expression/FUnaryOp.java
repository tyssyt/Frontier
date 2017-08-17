package tys.frontier.code.expression;

import tys.frontier.code.FClass;

public class FUnaryOp implements FExpression {

    private final FExpression expression;
    private final Operator operator;

    public FUnaryOp(FExpression expression, Operator operator) {
        this.expression = expression;
        this.operator = operator;
    }

    @Override
    public FClass getType() {
        return null; //TODO
    }

    public enum Operator { //TODO type
        NOT("!", true),
        PRE_INCREMENT("++", true),
        PRE_DECREMENT("--", true),
        POST_INCREMENT("++", false),
        POST_ODECREMENT("--", false),
        PLUS("+", true),
        MINUS("-", true);

        public final String stringRepresentation;
        public boolean prefix;

        Operator(String s, boolean prefix) {
            stringRepresentation = s;
            this.prefix = prefix;
        }

        public static Operator fromString (String string, boolean prefix) {
            for (Operator op : values()) {
                if (op.prefix==prefix && op.stringRepresentation.equals(string))
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
