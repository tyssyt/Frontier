package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FField;

public class DelegateFromTypeVar extends SyntaxError {

    public final FField delegatingField;

    public DelegateFromTypeVar(FField delegatingField) {
        super("Field " + delegatingField.getIdentifier() + " is delegating to Type Variable " + delegatingField.getType().getIdentifier());
        this.delegatingField = delegatingField;
    }
}
