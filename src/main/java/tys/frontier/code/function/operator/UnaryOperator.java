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

public enum UnaryOperator implements Operator {
    NOT(getParserToken(FrontierLexer.EXMARK), new AttributeIdentifier("!_")),
    NEG(getParserToken(FrontierLexer.SUB),    new AttributeIdentifier("-_"));

    private static final ImmutableMap<String, UnaryOperator> parserTokenMap =
            Arrays.stream(values()).collect(toImmutableMap(o -> o.parserToken, o -> o));

    public final String parserToken;
    public final AttributeIdentifier identifier;

    UnaryOperator(String parserToken, AttributeIdentifier identifier) {
        this.parserToken = parserToken;
        this.identifier = identifier;
    }

    public static UnaryOperator getFromParserToken(String parserToken) {
        return parserTokenMap.get(parserToken);
    }

    @Override
    public AttributeIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public boolean isUserDefinable() {
        return true;
    }

    public FFunction getFunction(FClass fClass) {
        return Iterables.getOnlyElement(fClass.getFunctions(false).get(identifier)).getFunction();
    }

    public FFunction createPredefined(FClass memberOf, FClass ret) {
        ImmutableList<FParameter> params = ImmutableList.of(FParameter.create(AttributeIdentifier.THIS, memberOf, false));
        return new FBaseFunction(identifier, memberOf, memberOf.getVisibility(), false, ret, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}
