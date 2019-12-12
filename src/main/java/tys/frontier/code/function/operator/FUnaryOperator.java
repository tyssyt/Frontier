package tys.frontier.code.function.operator;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FParameter;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.parser.antlr.FrontierLexer;

import static tys.frontier.code.function.operator.Operator.getParserToken;

public class FUnaryOperator extends FOperator {

    public enum Pre implements Operator {
        NOT(getParserToken(FrontierLexer.EXMARK), new FFunctionIdentifier("!_")),
        NEG(getParserToken(FrontierLexer.SUB),    new FFunctionIdentifier("-_")),
        INC(getParserToken(FrontierLexer.INC),    new FFunctionIdentifier("++_")),
        DEC(getParserToken(FrontierLexer.DEC),    new FFunctionIdentifier("--_"));

        private static final ImmutableList<Pre> values = ImmutableList.copyOf(values());

        public final String parserToken;
        public final FFunctionIdentifier identifier;

        Pre(String parserToken, FFunctionIdentifier identifier) {
            this.parserToken = parserToken;
            this.identifier = identifier;
        }

        public FUnaryOperator create(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass);
        }

        public FUnaryOperator createPredefined(FClass fClass) {
            return new FUnaryOperator(identifier, fClass, fClass) {
                {predefined = true;}
            };
        }

        public static Pre getFromParserToken(String parserToken) {
            for (Pre value : values)
                if (value.parserToken.equals(parserToken))
                    return value;
            return null;
        }

        @Override
        public FFunctionIdentifier getIdentifier() {
            return identifier;
        }

        @Override
        public boolean isUserDefinable() {
            return true;
        }

        public FFunction getFunction(FClass fClass) {
            return Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }
    }

    private FUnaryOperator(FFunctionIdentifier identifier, FClass fClass, FClass returnType) {
        super(identifier, fClass, returnType, ImmutableList.of(FParameter.create(FVariableIdentifier.THIS, fClass, false)));
    }
}
