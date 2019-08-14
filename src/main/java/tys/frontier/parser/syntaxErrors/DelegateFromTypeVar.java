package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FField;

public class DelegateFromTypeVar extends SyntaxError {

    public final FField delegatingField;

    public DelegateFromTypeVar(FField delegatingField) {
        super("Class " + delegatingField.getMemberOf().getIdentifier() + " is delegating to Type Variable " + delegatingField.getIdentifier());
        this.delegatingField = delegatingField;
    }
}
