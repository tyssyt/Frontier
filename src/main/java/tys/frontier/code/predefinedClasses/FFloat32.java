package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FIdentifier;

public class FFloat32 extends FPredefinedClass {

    public static final FFloat32 INSTANCE = new FFloat32();

    @Override
    public boolean canImplicitlyCast() {
        return true; //to float64
    }

    private FFloat32 () {
        super(FIdentifier.FLOAT32);
        addPredefinedFunctionsForArithType();
    }
}
