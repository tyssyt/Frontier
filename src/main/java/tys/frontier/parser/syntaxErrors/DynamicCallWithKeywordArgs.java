package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;

public class DynamicCallWithKeywordArgs extends SyntaxError {

    public final FExpression dynamicCall;
    public final ListMultimap<FIdentifier, FExpression> keywordArguments;

    public DynamicCallWithKeywordArgs(FExpression dynamicCall, ListMultimap<FIdentifier, FExpression> keywordArguments) {
        super(dynamicCall.getPosition(), "dynamic function call with keyword arguments: " + dynamicCall + ", " + keywordArguments);
        this.dynamicCall = dynamicCall;
        this.keywordArguments = keywordArguments;
    }
}
