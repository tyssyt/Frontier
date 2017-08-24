package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableSet;
import tys.frontier.code.FClass;

public final class FPredefinedClasses {

    public static final ImmutableSet<FClass> intTypes =
            ImmutableSet.of(FInt.INSTANCE, FInt32.INSTANCE, FInt64.INSTANCE);

    private FPredefinedClasses() {}

}
