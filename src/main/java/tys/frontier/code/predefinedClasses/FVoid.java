package tys.frontier.code.predefinedClasses;

import tys.frontier.code.identifier.FClassIdentifier;

public class FVoid extends FPredefinedClass {

    public static final FVoid INSTANCE = new FVoid();

    private FVoid () {
        super(FClassIdentifier.VOID);
    }

}
