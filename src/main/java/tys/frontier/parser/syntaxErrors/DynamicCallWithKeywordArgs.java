package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;

import java.util.Map;

public class DynamicCallWithKeywordArgs extends SyntaxError {

    public final FExpression dynamicCall;
    public final Map<FIdentifier, FExpression> keywordArguments;

    public DynamicCallWithKeywordArgs(FExpression dynamicCall, Map<FIdentifier, FExpression> keywordArguments) {
        super("dynamic function call with keyword arguments: " + dynamicCall + ", " + keywordArguments);
        this.dynamicCall = dynamicCall;
        this.keywordArguments = keywordArguments;
    }
}
