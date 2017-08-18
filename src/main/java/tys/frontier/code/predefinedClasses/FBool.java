package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FBool extends FPredefinedClass {

    public static final FBool INSTANCE = new FBool();

    private FBool() {
        super(FClassIdentifier.BOOL);
    }
}
