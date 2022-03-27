package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.parser.antlr.FrontierLexer;
import tys.frontier.util.Pair;

import static com.google.common.collect.Lists.asList;

public class Access implements Operator {
    public static final FIdentifier ID = new FIdentifier("[]");
    public static final Access INSTANCE = new Access();
    public static final String PARSER_TOKEN = Operator.getParserToken(FrontierLexer.ARRAY);

    private Access() {}

    @Override
    public FIdentifier getIdentifier() {
        return ID;
    }

    @Override
    public boolean isUserDefinable() {
        return true;
    }

    public static Pair<FFunction, FFunction> createPredefined(FClass memberOf, FType value, FClass... keys) {
        FunctionBuilder builder = new FunctionBuilder(ID, memberOf.getNamespace())
                .setVisibility(memberOf.getNamespace().getVisibility()).setPredefined(true).setParams(asList(memberOf, keys)).setReturnType(value);
        FBaseFunction getter = builder.build();

        //TODO @PositionForGeneratedCode
        ImmutableList<FParameter> assignees = ImmutableList.of(FParameter.create(null, new FIdentifier("value"), value, false));
        FBaseFunction setter = builder.setAssignees(assignees).build();
        return new Pair<>(getter, setter);
    }
}
