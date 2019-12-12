package tys.frontier.code.function.operator;

import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.util.Utils;

import java.util.List;

public interface Operator {

    FFunctionIdentifier getIdentifier();
    boolean isUserDefinable();

    static Operator get(String stringRepresentation, List<FType> argTypes) {
        Operator operator = null;
        if (argTypes.size() == 1) {
            operator = FUnaryOperator.Pre.getFromParserToken(stringRepresentation);
        } else if (argTypes.size() == 2) {
            operator = FBinaryOperator.Arith.getFromParserToken(stringRepresentation);
            if (operator == null)
                operator = FBinaryOperator.Bool.getFromParserToken(stringRepresentation);
        }

        if (operator == null)
            return Utils.NYI("unknown Operator: " + stringRepresentation + ", " + argTypes); //TODO proper error
        return operator;
    }

    static String getParserToken(int tokenType) {
        String name = FrontierLexer.VOCABULARY.getLiteralName(tokenType);
        return name.substring(1, name.length()-1);
    }

}