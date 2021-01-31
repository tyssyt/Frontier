package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;

import java.util.List;

public class WrongNumberOfIdentifiersInFor extends SyntaxError {

    public final ImmutableList<FIdentifier> identifiers;
    public final ImmutableList<FType> iteratedTypes;

    public WrongNumberOfIdentifiersInFor(Position position, List<FIdentifier> identifiers, List<FType> iteratedTypes) {
        super(position, "For Loop with " + identifiers.size() + " identifiers and " + iteratedTypes.size() + " types. Identifiers: " + identifiers);
        this.identifiers = ImmutableList.copyOf(identifiers);
        this.iteratedTypes = ImmutableList.copyOf(iteratedTypes);
    }
}
