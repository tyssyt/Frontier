package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FInt32 extends FPredefinedClass {

    public static final FInt32 INSTANCE = new FInt32();

    private FInt32 () {
        super(FClassIdentifier.INT32);
    }

}
