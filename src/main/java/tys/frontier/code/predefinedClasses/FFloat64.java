package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FFloat64 extends FPredefinedClass {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {
        super(FClassIdentifier.FLOAT64);
    }

}
