package tys.frontier.code.namespace;

import tys.frontier.code.expression.UnboundExpression;
import tys.frontier.code.functionResolve.FunctionResolver;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;

import java.util.List;
import java.util.Map;

public interface Namespace extends IdentifierNameable {

    FType getType();
    Location getLocation();
    FIdentifier nextReturnTypeIdentifier();
    FunctionResolver.Result resolveFunction(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve, List<UnboundExpression> unbounds) throws FunctionNotFound;
}
