package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.expression.FExpression;

public class NonOptionalExMark extends SyntaxError {

    public final FExpression nonOptional;

    public NonOptionalExMark(FExpression nonOptional) {
        super("used \'!\' operator on non optional expression: " + nonOptional + ", which is of type: " + nonOptional.getType());
        this.nonOptional = nonOptional;
    }
}
