package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FInt extends FPredefinedClass {

    public static final FInt INSTANCE = new FInt();

    private FInt () {
        super(FClassIdentifier.INT);
    }

}
