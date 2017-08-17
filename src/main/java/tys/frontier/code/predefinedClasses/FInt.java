package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public class FInt extends FClass {

    public static final FInt INSTANCE = new FInt();

    private FInt () {
        super(FClassIdentifier.INT, FVisibilityModifier.PUBLIC);
    }

}
