package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FType extends FPredefinedClass {

    public static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("!Type");

    public static final FType INSTANCE = new FType();

    private FType () {
        super(IDENTIFIER);
    }
}
