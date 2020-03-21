package tys.frontier.code.statement;

import com.google.common.base.Joiner;
import tys.frontier.code.FLocalVariable;

import java.util.List;

import static java.util.Arrays.asList;

public class FLambdaBlock extends FBlock {

    private List<FLocalVariable> variables;

    private FLambdaBlock(List<FStatement> statements, List<FLocalVariable> variables) {
        super(statements);
        assert !variables.isEmpty();
        this.variables = variables;
    }

    public static FLambdaBlock from(List<FLocalVariable> variables, FStatement... statements) {
        return new FLambdaBlock(asList(statements), variables);
    }

    public static FLambdaBlock from(List<FStatement> statements, List<FLocalVariable> variables) {
        return new FLambdaBlock(statements, variables);
    }

    public List<FLocalVariable> getVariables() {
        return variables;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("{ \\ ");
        Joiner.on(", ").appendTo(sb, variables).append(" ->\n");
        for (FStatement statement : this)
            statement.toString(sb).append('\n');
        return sb.append('}');
    }
}
