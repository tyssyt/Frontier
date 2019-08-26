package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.util.StringBuilderStringCollector;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Map;

public class FunctionNotFound extends SyntaxError {

    public final FFunctionIdentifier identifier;
    public final ImmutableList<FType> positionalArgs;
    public final ImmutableMap<FIdentifier, FType> keywordArgs;

    public FunctionNotFound(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs) {
        super("Function not found: "
                + identifier + '(' + Utils.joinIdentifiers(new StringBuilder(), positionalArgs, ",")
                + keywordArgs.entrySet().stream().collect(new StringBuilderStringCollector<>(
                        (sb, e) -> sb.append(',').append(e.getKey()).append('=').append(e.getValue().getIdentifier()
                ))));
        this.identifier = identifier;
        this.positionalArgs = ImmutableList.copyOf(positionalArgs);
        this.keywordArgs = ImmutableMap.copyOf(keywordArgs);
    }
}
