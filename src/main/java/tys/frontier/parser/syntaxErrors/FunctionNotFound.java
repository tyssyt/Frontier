package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.type.FType;

import java.util.List;
import java.util.stream.Collectors;

public class FunctionNotFound extends SyntaxError {

    public final FFunctionIdentifier identifier;
    public final ImmutableList<FType> paramTypes;

    public FunctionNotFound(FFunctionIdentifier identifier, List<FType> paramTypes) {
        super("" + identifier + paramTypes.stream().map(FType::getIdentifier).collect(Collectors.toList()).toString());
        this.identifier = identifier;
        this.paramTypes = ImmutableList.copyOf(paramTypes);
    }
}
