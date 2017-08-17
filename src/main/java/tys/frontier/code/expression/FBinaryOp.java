package tys.frontier.code.expression;

public class FBinaryOp implements FExpression {

    public final FExpression first;
    public final FExpression second;
    public final Operator operator;
    public FBinaryOp(FExpression first, FExpression second, Operator operator) {
        this.first = first;
        this.second = second;
        this.operator = operator;
    }

    public  enum Operator { //TODO type restrictions of operators
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
