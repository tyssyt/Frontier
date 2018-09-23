package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FField;

public class CyclicDelegate extends SyntaxError {

    public final FField field;

    public CyclicDelegate(FField field) {
        super("Cyclic Delegate starting at: " + field);
        this.field = field;
    }
}
