package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FField;
import tys.frontier.code.FInterface;

public class InterfaceInstanceField extends SyntaxError {

    public final FInterface fInterface;
    public final FField field;

    public InterfaceInstanceField(FInterface fInterface, FField field) {
        super("Interface " + fInterface.getIdentifier() + " has instance field " + field.getIdentifier());
        this.fInterface = fInterface;
        this.field = field;
    }
}
