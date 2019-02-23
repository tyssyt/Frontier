package tys.frontier.code;

import com.google.common.collect.Multimap;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.StringBuilderToString;

import java.util.List;

public interface FType extends IdentifierNameable, StringBuilderToString {

    FFunction resolveFunction (FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound;

    FFunction resolveFunction (FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound;

    @Override
    FTypeIdentifier getIdentifier();
}
