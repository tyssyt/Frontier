package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;

import java.util.List;

public class WrongNumberOfIdentifiersInFor extends SyntaxError {

    public final ImmutableList<FIdentifier> identifiers;
    public final ImmutableList<FType> iteratedTypes;

    public WrongNumberOfIdentifiersInFor(List<AttributeIdentifier> identifiers, List<FType> iteratedTypes) {
        super("For Loop with " + identifiers.size() + " identifiers and " + iteratedTypes.size() + " types. Identifiers: " + identifiers);
        this.identifiers = ImmutableList.copyOf(identifiers);
        this.iteratedTypes = ImmutableList.copyOf(iteratedTypes);
    }
}
