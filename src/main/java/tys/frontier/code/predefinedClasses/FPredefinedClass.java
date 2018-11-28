package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.passes.analysis.reachability.Reachability;

public abstract class FPredefinedClass extends FClass {

    public FPredefinedClass(FTypeIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT);
    }

    @Override
    public void removeUnreachable(Reachability.ReachableClass reachable) {}
}
