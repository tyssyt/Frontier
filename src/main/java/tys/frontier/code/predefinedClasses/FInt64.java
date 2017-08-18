package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FInt64 extends FPredefinedClass{

    public static final FInt64 INSTANCE = new FInt64();

    private FInt64 () {
        super(FClassIdentifier.INT64);
    }

}
