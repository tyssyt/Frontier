package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.List;

public class HasSelfCall extends HasCall {

    public HasSelfCall(FExpression origin, FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, boolean lhsResolve, FTypeVariable in) {
        super(origin, identifier, positionalArgs, keywordArgs, lhsResolve);
    }

    FunctionResolver.Result resolve(DefaultNamespace namespace) throws FunctionNotFound {
        return namespace.softResolveFunction(getIdentifier(), getPositionalArgs(), getKeywordArgs(), null, isLhsResolve());
    }
}
