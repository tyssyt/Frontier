package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FType extends FPredefinedClass {

    public static final FType INSTANCE = new FType();

    public static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("!Type");

    private FType () {
        super(IDENTIFIER);
    }
}
