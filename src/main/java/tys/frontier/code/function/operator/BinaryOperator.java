package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierLexer;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
import static java.util.Collections.emptyMap;
import static tys.frontier.code.function.operator.Operator.getParserToken;

public enum BinaryOperator implements Operator {
    //Arithmetic operators
    PLUS   (getParserToken(FrontierLexer.ADD),   new FFunctionIdentifier("+")),
    MINUS  (getParserToken(FrontierLexer.SUB),   new FFunctionIdentifier("-")),
    TIMES  (getParserToken(FrontierLexer.STAR),  new FFunctionIdentifier("*")),
    DIVIDED(getParserToken(FrontierLexer.SLASH), new FFunctionIdentifier("/")),
    MODULO (getParserToken(FrontierLexer.MOD),   new FFunctionIdentifier("%")),
    AAND    (getParserToken(FrontierLexer.AAND),  new FFunctionIdentifier("&")),
    AOR     (getParserToken(FrontierLexer.AOR),   new FFunctionIdentifier("|")),
    XOR    (getParserToken(FrontierLexer.XOR),   new FFunctionIdentifier("^")),

    //Boolean Operators
    AND                 (getParserToken(FrontierLexer.AND),                new FFunctionIdentifier("&&")),
    OR                  (getParserToken(FrontierLexer.OR),                 new FFunctionIdentifier("||")),
    EQUALS              (getParserToken(FrontierLexer.EQUAL),              new FFunctionIdentifier("==")),
    EQUALS_ID           (getParserToken(FrontierLexer.EQUAL_ID),           new FFunctionIdentifier("=*=")),
    EQUALS_CONTAINER    (getParserToken(FrontierLexer.EQUAL_CONTAINER),    new FFunctionIdentifier("=[]=")),
    NOT_EQUALS          (getParserToken(FrontierLexer.NOTEQUAL),           new FFunctionIdentifier("=!=")),
    NOT_EQUALS_ID       (getParserToken(FrontierLexer.NOTEQUAL_ID),        new FFunctionIdentifier("=!*=")),
    NOT_EQUALS_CONTAINER(getParserToken(FrontierLexer.NOTEQUAL_CONTAINER), new FFunctionIdentifier("=![]=")),
    LESS                (getParserToken(FrontierLexer.LT),                 new FFunctionIdentifier("<")),
    GREATER             (getParserToken(FrontierLexer.GT),                 new FFunctionIdentifier(">")),
    LESS_EQUAL          (getParserToken(FrontierLexer.LE),                 new FFunctionIdentifier("<=")),
    GREATER_EQUAL       (getParserToken(FrontierLexer.GE),                 new FFunctionIdentifier(">="));

    private static final ImmutableMap<String, BinaryOperator> parserTokenMap =
            Arrays.stream(values()).collect(toImmutableMap(o -> o.parserToken, o -> o));

    public final String parserToken;
    public final FFunctionIdentifier identifier;

    BinaryOperator(String parserToken, FFunctionIdentifier identifier) {
        this.parserToken = parserToken;
        this.identifier = identifier;
    }

    public static BinaryOperator getFromParserToken(String parserToken) {
        return parserTokenMap.get(parserToken);
    }

    @Override
    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public boolean isUserDefinable() {
        switch (this) {
            case EQUALS_ID: case NOT_EQUALS_ID:
                return false;
            default:
                return true;
        }
    }

    public FFunction getFunction(FClass fClass) {
        return Iterables.getOnlyElement(fClass.getFunctions(false).get(identifier)).getFunction();
    }

    public FFunction createPredefined(FClass memberOf, FClass second, FClass ret) {
        ImmutableList<FParameter> params = ImmutableList.of(
                FParameter.create(new FVariableIdentifier("first"), memberOf, false),
                FParameter.create(new FVariableIdentifier("second"), second, false)
        );
        return new FBaseFunction(identifier, memberOf, memberOf.getVisibility(), false, ret, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}