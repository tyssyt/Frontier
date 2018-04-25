package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public abstract class FPredefinedClass extends FClass {

    public FPredefinedClass(FClassIdentifier identifier) {
        super(identifier, FVisibilityModifier.PUBLIC, false);
    }

    @Override
    public void addField(FField field) {
        throw new RuntimeException("somthing went terribly wrong");
    }
}
