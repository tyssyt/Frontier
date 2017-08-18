package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FFloat32 extends FPredefinedClass {

    public static final FFloat32 INSTANCE = new FFloat32();

    private FFloat32 () {
        super(FClassIdentifier.FLOAT32);
    }

}
