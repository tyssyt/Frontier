package tys.frontier.code.namespace;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

import java.util.List;

public interface Namespace extends IdentifierNameable {

    FType getType();

    Location getLocation();

    FIdentifier nextReturnTypeIdentifier();

    FFunction getOpen(FIdentifier identifier);

    void addRemoteFunction(FFunction fFunction);

    default FunctionResolver.Result hardResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        FunctionResolver.Result res = softResolveFunction(identifier, positionalArgs, keywordArgs, returnType, lhsResolve);
        try {
            TypeConstraint.addAll(res.constraints);
        } catch (UnfulfillableConstraints unfulfillableConstraints) {
            throw new FunctionNotFound(getLocation().getPoint(), identifier, positionalArgs, keywordArgs);
        }
        return res;
    }

    FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound;
}
