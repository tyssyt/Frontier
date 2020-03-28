package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.List;

public class HasRemoteCall extends HasCall {

    private Namespace in;

    public HasRemoteCall(FExpression origin, FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, boolean lhsResolve, Namespace in) {
        super(origin, identifier, positionalArgs, keywordArgs, lhsResolve);
        this.in = in;
    }

    @Override
    FunctionResolver.Result resolve(TypeInstantiation typeInstantiation) throws FunctionNotFound {
        return in.softResolveFunction(getIdentifier(), getPositionalArgs(), getKeywordArgs(), null, isLhsResolve());
    }
}
