package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public class FFloat64 extends FClass {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {
        super(FClassIdentifier.FLOAT64, FVisibilityModifier.PUBLIC);
    }

}
