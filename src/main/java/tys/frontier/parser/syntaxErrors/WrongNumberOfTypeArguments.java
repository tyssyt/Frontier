package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClass;
import tys.frontier.code.FType;

import java.util.List;

public class WrongNumberOfTypeArguments extends SyntaxError {

    public final FClass fClass;
    public final ImmutableList<FType> arguments;

    public WrongNumberOfTypeArguments(FClass fClass, List<FType> arguments) {
        super("Wrong number of Type arguments for class " + fClass.getIdentifier() +
                " expected " +fClass.getParameters().size() + " but got " + arguments.size());
        this.fClass = fClass;
        this.arguments = ImmutableList.copyOf(arguments);
    }
}
