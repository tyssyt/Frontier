package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FTypeIdentifier;

public abstract class FPredefinedClass extends FClass {

    public FPredefinedClass(FTypeIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT);
    }
}
