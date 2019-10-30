package tys.frontier.code.type;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.FField;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.StringBuilderToString;

import java.util.List;

public interface FType extends IdentifierNameable, StringBuilderToString {

    long concreteness(); //TODO there probably is an established term for this

    boolean canImplicitlyCast();

    default FunctionResolver.Result hardResolveFunction(FFunctionIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType) throws FunctionNotFound {
        FunctionResolver.Result res = softResolveFunction(identifier, positionalArgs, keywordArgs, returnType);
        try {
            TypeConstraint.addAll(res.constraints);
        } catch (UnfulfillableConstraints unfulfillableConstraints) {
            throw new FunctionNotFound(identifier, positionalArgs, keywordArgs);
        }
        return res;
    }

    FunctionResolver.Result softResolveFunction(FFunctionIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType) throws FunctionNotFound;

    FField getField(FIdentifier identifier) throws FieldNotFound;

    @Override
    FTypeIdentifier getIdentifier();
}
