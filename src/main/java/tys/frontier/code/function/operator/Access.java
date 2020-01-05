package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.util.Pair;

import static java.util.Collections.emptyMap;

public class Access implements Operator {
    public static final AttributeIdentifier ID = new AttributeIdentifier("[]");
    public static final Access INSTANCE = new Access();
    public static final String PARSER_TOKEN = Operator.getParserToken(FrontierLexer.Array);

    private Access() {}

    @Override
    public AttributeIdentifier getIdentifier() {
        return ID;
    }

    @Override
    public boolean isUserDefinable() {
        return true;
    }

    public static Pair<FFunction, FFunction> createPredefined(FClass memberOf, FClass key, FType value) {
        FParameter p1 = FParameter.create(AttributeIdentifier.THIS, memberOf, false);
        FParameter p2 = FParameter.create(new AttributeIdentifier("key"), key, false);
        ImmutableList<FParameter> params = ImmutableList.of(p1, p2);
        FBaseFunction getter = new FBaseFunction(ID, memberOf, memberOf.getVisibility(), false, value, params, null, emptyMap()) {
            {predefined = true;}
        };

        ImmutableList<FParameter> assignees = ImmutableList.of(FParameter.create(new AttributeIdentifier("value"), value, false));
        FBaseFunction setter = new FBaseFunction(ID, memberOf, memberOf.getVisibility(), false, FTuple.VOID, params, assignees, emptyMap()) {
            {predefined = true;}
        };
        return new Pair<>(getter, setter);
    }
}
