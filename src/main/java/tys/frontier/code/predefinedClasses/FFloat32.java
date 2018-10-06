package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FTypeIdentifier;

public class FFloat32 extends FPredefinedClass {

    public static final FFloat32 INSTANCE = new FFloat32();

    private FFloat32 () {
        super(FTypeIdentifier.FLOAT32);
    }
}
