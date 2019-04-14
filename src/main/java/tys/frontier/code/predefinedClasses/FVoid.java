package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FVoid extends FPredefinedClass {

    public static final FVoid INSTANCE = new FVoid();

    private FVoid () {
        super(FTypeIdentifier.VOID);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}
