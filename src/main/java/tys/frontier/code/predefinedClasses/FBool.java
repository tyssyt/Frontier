package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public class FBool extends FClass {

    public static final FBool INSTANCE = new FBool();

    private FBool() {
        super(FClassIdentifier.BOOL, FVisibilityModifier.PUBLIC);
    }
}
