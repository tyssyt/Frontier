package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.StaticField;

public class DelegateFromStaticField extends SyntaxError {

    public final StaticField delegatingField;

    public DelegateFromStaticField(StaticField delegatingField) {
        super("Field " + delegatingField.getIdentifier() + " is static and delegating");
        this.delegatingField = delegatingField;
    }
}
