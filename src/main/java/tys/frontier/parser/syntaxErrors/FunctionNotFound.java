package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimaps;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Utils;

import java.util.List;

import static tys.frontier.util.MyCollectors.sbToString;

public class FunctionNotFound extends SyntaxError {

    public final FIdentifier identifier;
    public final ImmutableList<FType> positionalArgs;
    public final ImmutableListMultimap<FIdentifier, FType> keywordArgs;


    public FunctionNotFound(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs) {
        super("Function not found: "
                + identifier + '(' + Utils.joinIdentifiers(new StringBuilder(), positionalArgs, ",")
                + Multimaps.asMap(keywordArgs).entrySet().stream().collect(sbToString(
                (sb, e) -> sb.append(',').append(e.getKey()).append('=').append(e.getValue()))) + ')');
        this.identifier = identifier;
        this.positionalArgs = ImmutableList.copyOf(positionalArgs);
        this.keywordArgs = ImmutableListMultimap.copyOf(keywordArgs);
    }

    public FunctionNotFound(Position position, FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs) {
        super(position, "Function not found: "
                + identifier + '(' + Utils.joinIdentifiers(new StringBuilder(), positionalArgs, ",")
                + Multimaps.asMap(keywordArgs).entrySet().stream().collect(sbToString(
                        (sb, e) -> sb.append(',').append(e.getKey()).append('=').append(e.getValue()))) + ')');
        this.identifier = identifier;
        this.positionalArgs = ImmutableList.copyOf(positionalArgs);
        this.keywordArgs = ImmutableListMultimap.copyOf(keywordArgs);
    }
}
