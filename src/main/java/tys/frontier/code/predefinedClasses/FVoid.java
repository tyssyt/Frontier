package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public class FVoid extends FClass {

    public static final FVoid INSTANCE = new FVoid();

    private FVoid () {
        super(FClassIdentifier.VOID, FVisibilityModifier.PUBLIC);
    }

}
