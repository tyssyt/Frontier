package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.List;
import java.util.stream.Collectors;

public class FunctionNotFound extends SyntaxError {

    public final FFunctionIdentifier identifier;
    public final ImmutableList<FClass> paramTypes;

    public FunctionNotFound(FFunctionIdentifier identifier, List<FClass> paramTypes) {
        super("" + identifier + paramTypes.stream().map(FClass::getIdentifier).collect(Collectors.toList()).toString());
        this.identifier = identifier;
        this.paramTypes = ImmutableList.copyOf(paramTypes);
    }
}
