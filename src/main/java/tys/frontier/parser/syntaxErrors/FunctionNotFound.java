package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Map;

import static tys.frontier.util.MyCollectors.sbToString;

public class FunctionNotFound extends SyntaxError {

    public final FIdentifier identifier;
    public final ImmutableList<FType> positionalArgs;
    public final Map<FIdentifier, FType> keywordArgs;


    public FunctionNotFound(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs) {
        super("Function not found: "
                + identifier + '(' + Utils.joinIdentifiers(new StringBuilder(), positionalArgs, ",")
                + keywordArgs.entrySet().stream().collect(sbToString(
                (sb, e) -> sb.append(',').append(e.getKey()).append('=').append(e.getValue()))) + ')');
        this.identifier = identifier;
        this.positionalArgs = ImmutableList.copyOf(positionalArgs);
        this.keywordArgs = ImmutableMap.copyOf(keywordArgs);
    }

    public FunctionNotFound(Position position, FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs) {
        super(position, "Function not found: "
                + identifier + '(' + Utils.joinIdentifiers(new StringBuilder(), positionalArgs, ",")
                + keywordArgs.entrySet().stream().collect(sbToString(
                        (sb, e) -> sb.append(',').append(e.getKey()).append('=').append(e.getValue()))) + ')');
        this.identifier = identifier;
        this.positionalArgs = ImmutableList.copyOf(positionalArgs);
        this.keywordArgs = ImmutableMap.copyOf(keywordArgs);
    }
}
