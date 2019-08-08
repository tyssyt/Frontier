package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.type.FType;

import java.util.List;

public class WrongNumberOfTypeArguments extends SyntaxError {

    public final IdentifierNameable fClass;
    public final ImmutableList<FType> arguments;
    public final int expectedNumberOfArgs;

    public WrongNumberOfTypeArguments(IdentifierNameable fClass, List<FType> arguments, int expectedNumberOfArgs) {
        super("Wrong number of Type arguments for class " + fClass.getIdentifier() +
                " expected " + expectedNumberOfArgs + " but got " + arguments.size());
        this.fClass = fClass;
        this.arguments = ImmutableList.copyOf(arguments);
        this.expectedNumberOfArgs = expectedNumberOfArgs;
    }
}
