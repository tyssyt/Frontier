package tys.frontier.code.expression;

import com.google.common.collect.ImmutableMap;
import tys.frontier.code.FClass;
import tys.frontier.code.statement.NeedsTypeCheck;
import tys.frontier.parser.syntaxTree.syntaxErrors.IncompatibleTypes;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;

public class FBinaryOp implements FExpression, NeedsTypeCheck {

    private FExpression first;
    private FExpression second;
    private Operator operator;
    public FBinaryOp(FExpression first, FExpression second, Operator operator) throws IncompatibleTypes {
        this.first = first;
        this.second = second;
        this.operator = operator;
        checkTypes();
    }

    @Override
    public FClass getType() {
        return null; //TODO
    }

    @Override
    public void checkTypes() throws IncompatibleTypes {

    }

    public enum Operator { //TODO type restrictions of operators
        EQ("=="),  //always exists, return type bool, input same type or promotable
        NE("!="),  //same as above
        AND("&&"), //bool + bool -> bool, orsomthing that promotes to bool?
        OR("||"),  //same as above
        XOR("^"),  //same as above
        LE("<="),  //return type bool, inputs need to be comparable
        GE("<="),  //same as above
        LT("<"),   //same as above
        GT(">"),   //same as above
        MUL("*"),  //needs to be overridden properly, or int & int -> max of both types
        DIV("/"),  //same as above
        MOD("%"),  //same as above
        ADD("+"),  //same as above
        SUB("-");  //same as above

        public final String stringRepresentation;

        private static ImmutableMap<String, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> o.stringRepresentation, o -> o));

        Operator(String s) {
            stringRepresentation = s;
        }

        public static Operator fromString (String string) {
            return stringMap.get(string);
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}
