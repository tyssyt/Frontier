package tys.frontier.code.statement;

import tys.frontier.code.FLocalVariable;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Joiners;

import java.util.List;

import static java.util.Arrays.asList;

public class FLambdaBlock extends FBlock {

    private List<FLocalVariable> variables;

    private FLambdaBlock(Position position, List<FStatement> statements, List<FLocalVariable> variables) {
        super(position, statements);
        assert !variables.isEmpty();
        this.variables = variables;
    }

    public static FLambdaBlock from(Position position, List<FLocalVariable> variables, FStatement... statements) {
        return new FLambdaBlock(position, asList(statements), variables);
    }

    public static FLambdaBlock from(Position position, List<FStatement> statements, List<FLocalVariable> variables) {
        return new FLambdaBlock(position, statements, variables);
    }

    public List<FLocalVariable> getVariables() {
        return variables;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("{ \\ ");
        Joiners.ON_COMMA.appendTo(sb, variables).append(" ->\n");
        for (FStatement statement : this)
            statement.toString(sb).append('\n');
        return sb.append('}');
    }
}
