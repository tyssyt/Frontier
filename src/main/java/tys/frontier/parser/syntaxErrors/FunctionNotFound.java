package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FFunctionIdentifier;

import java.util.List;

public class FunctionNotFound extends SyntaxError {

    public final FFunctionIdentifier identifier;
    public final ImmutableList<FClass> paramTypes;

    public FunctionNotFound(FFunctionIdentifier identifier, List<FClass> paramTypes) {
        super("" + identifier + paramTypes);
        this.identifier = identifier;
        this.paramTypes = ImmutableList.copyOf(paramTypes);
    }
}
