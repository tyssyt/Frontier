package tys.frontier.code.type;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.StringBuilderToString;

import java.util.List;
import java.util.Map;

public interface FType extends IdentifierNameable, StringBuilderToString {

    long concreteness(); //TODO there probably is an established term for this

    boolean canImplicitlyCast();

    default FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, FType returnType, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        ArrayListMultimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();
        FFunction res = resolveFunction(identifier, argumentTypes, returnType, typeInstantiation, constraints);
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : constraints.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new FunctionNotFound(identifier, argumentTypes);
        }
        return res;
    }

    FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> argumentTypes, FType returnType, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound;

    FField getField(FIdentifier identifier) throws FieldNotFound;

    @Override
    FTypeIdentifier getIdentifier();
}
