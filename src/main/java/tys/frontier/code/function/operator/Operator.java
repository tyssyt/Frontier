package tys.frontier.code.function.operator;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.util.Utils;

public interface Operator {

    FIdentifier getIdentifier();
    boolean isUserDefinable();

    static Operator get(String stringRepresentation, int numberOfArgs) {
        Operator operator = null;
        if (stringRepresentation.equals(Access.PARSER_TOKEN))
            operator = Access.INSTANCE;
        else if (numberOfArgs == 1)
            operator = UnaryOperator.getFromParserToken(stringRepresentation);
        else if (numberOfArgs == 2)
            operator = BinaryOperator.getFromParserToken(stringRepresentation);

        if (operator == null)
            return Utils.NYI("unknown Operator: " + stringRepresentation + '(' + numberOfArgs + ')'); //TODO proper error
        return operator;
    }

    static String getParserToken(int tokenType) {
        String name = FrontierLexer.VOCABULARY.getLiteralName(tokenType);
        return name.substring(1, name.length()-1);
    }

}