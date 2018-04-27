package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public abstract class FPredefinedClass extends FClass {

    public FPredefinedClass(FClassIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT, false);
    }
}
