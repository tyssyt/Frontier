package tys.frontier.parser.syntaxTree.syntaxErrors;

import tys.frontier.code.FField;

public class StaticAccessToInstanceField extends SyntaxError {

    public final FField field;

    public StaticAccessToInstanceField(FField field) {
        super(field.toString());
        this.field = field;
    }
}
