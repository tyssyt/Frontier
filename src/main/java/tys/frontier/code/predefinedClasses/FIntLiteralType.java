package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FIdentifier;

public class FIntLiteralType extends FPredefinedClass {

    private static final FIdentifier IDENTIFIER = new FIdentifier("!FIntLiteral");

    public static final FIntLiteralType INSTANCE = new FIntLiteralType();

    private FIntLiteralType() {
        super(IDENTIFIER);
    }
}
