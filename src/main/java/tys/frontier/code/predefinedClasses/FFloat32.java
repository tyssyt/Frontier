package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public class FFloat32 extends FClass {

    public static final FFloat32 INSTANCE = new FFloat32();

    private FFloat32 () {
        super(FClassIdentifier.FLOAT32, FVisibilityModifier.PUBLIC);
    }

}
