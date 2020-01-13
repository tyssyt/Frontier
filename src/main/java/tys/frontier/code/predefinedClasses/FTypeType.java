package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FIdentifier;

public class FTypeType extends FPredefinedClass {

    public static final FIdentifier IDENTIFIER = new FIdentifier("!Type");

    public static final FTypeType INSTANCE = new FTypeType();

    private FTypeType() {
        super(IDENTIFIER);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}
