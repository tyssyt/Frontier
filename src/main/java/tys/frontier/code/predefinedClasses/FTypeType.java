package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FTypeType extends FPredefinedClass {

    public static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("!Type");

    public static final FTypeType INSTANCE = new FTypeType();

    private FTypeType() {
        super(IDENTIFIER);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}
