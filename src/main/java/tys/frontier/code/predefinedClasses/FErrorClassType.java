package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FErrorIdentifier;

public class FErrorClassType  extends FClass {
    public FErrorClassType(FErrorIdentifier identifier) {
        super(identifier, FVisibilityModifier.PUBLIC);
    }
}
