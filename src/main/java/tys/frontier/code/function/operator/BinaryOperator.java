package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierLexer;

import java.util.Arrays;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
import static java.util.Collections.emptyMap;
import static tys.frontier.code.function.operator.Operator.getParserToken;

public enum BinaryOperator implements Operator {
    //Arithmetic operators
    PLUS   (getParserToken(FrontierLexer.ADD),   new AttributeIdentifier("+")),
    MINUS  (getParserToken(FrontierLexer.SUB),   new AttributeIdentifier("-")),
    TIMES  (getParserToken(FrontierLexer.STAR),  new AttributeIdentifier("*")),
    DIVIDED(getParserToken(FrontierLexer.SLASH), new AttributeIdentifier("/")),
    MODULO (getParserToken(FrontierLexer.MOD),   new AttributeIdentifier("%")),
    AAND    (getParserToken(FrontierLexer.AAND),  new AttributeIdentifier("&")),
    AOR     (getParserToken(FrontierLexer.AOR),   new AttributeIdentifier("|")),
    XOR    (getParserToken(FrontierLexer.XOR),   new AttributeIdentifier("^")),

    //Boolean Operators
    AND                 (getParserToken(FrontierLexer.AND),                new AttributeIdentifier("&&")),
    OR                  (getParserToken(FrontierLexer.OR),                 new AttributeIdentifier("||")),
    EQUALS              (getParserToken(FrontierLexer.EQUAL),              new AttributeIdentifier("==")),
    EQUALS_ID           (getParserToken(FrontierLexer.EQUAL_ID),           new AttributeIdentifier("=*=")),
    EQUALS_CONTAINER    (getParserToken(FrontierLexer.EQUAL_CONTAINER),    new AttributeIdentifier("=[]=")),
    NOT_EQUALS          (getParserToken(FrontierLexer.NOTEQUAL),           new AttributeIdentifier("=!=")),
    NOT_EQUALS_ID       (getParserToken(FrontierLexer.NOTEQUAL_ID),        new AttributeIdentifier("=!*=")),
    NOT_EQUALS_CONTAINER(getParserToken(FrontierLexer.NOTEQUAL_CONTAINER), new AttributeIdentifier("=![]=")),
    LESS                (getParserToken(FrontierLexer.LT),                 new AttributeIdentifier("<")),
    GREATER             (getParserToken(FrontierLexer.GT),                 new AttributeIdentifier(">")),
    LESS_EQUAL          (getParserToken(FrontierLexer.LE),                 new AttributeIdentifier("<=")),
    GREATER_EQUAL       (getParserToken(FrontierLexer.GE),                 new AttributeIdentifier(">="));

    private static final ImmutableMap<String, BinaryOperator> parserTokenMap =
            Arrays.stream(values()).collect(toImmutableMap(o -> o.parserToken, o -> o));

    public final String parserToken;
    public final AttributeIdentifier identifier;

    BinaryOperator(String parserToken, AttributeIdentifier identifier) {
        this.parserToken = parserToken;
        this.identifier = identifier;
    }

    public static BinaryOperator getFromParserToken(String parserToken) {
        return parserTokenMap.get(parserToken);
    }

    @Override
    public AttributeIdentifier getIdentifier() {
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
                FParameter.create(new AttributeIdentifier("first"), memberOf, false),
                FParameter.create(new AttributeIdentifier("second"), second, false)
        );
        return new FBaseFunction(identifier, memberOf, memberOf.getVisibility(), false, ret, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}