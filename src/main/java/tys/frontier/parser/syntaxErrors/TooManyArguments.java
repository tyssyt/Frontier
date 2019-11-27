package tys.frontier.parser.syntaxErrors;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.type.FType;

import java.util.List;

public class TooManyArguments extends SyntaxError {

    public final ImmutableList<FType> given;
    public final ImmutableList<FType> expected;

    public final FType overFlow;

    public TooManyArguments(Iterable<FType> given, List<FType> expected) {
        super("Too many arguments given. Got: " + given + ", expected: " + expected);
        this.given = ImmutableList.copyOf(given);
        this.expected = ImmutableList.copyOf(expected);
        this.overFlow = null;
    }

    public TooManyArguments(FType overFlow) {
        super("Too many arguments given. Overflow at: " + overFlow);
        this.given = null;
        this.expected = null;
        this.overFlow = overFlow;
    }
}
