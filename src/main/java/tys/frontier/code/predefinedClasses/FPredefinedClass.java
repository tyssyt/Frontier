package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableSet;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;

public abstract class FPredefinedClass extends FClass {

    //TODO all functions added to a predefined class should be auto marked as predefined

    static {
        //make sure all important classes are loaded
        FBool bool = FBool.INSTANCE;
        FInt fInt = FInt.INSTANCE;
        FInt32 int32 = FInt32.INSTANCE;
        FInt64 int64 = FInt64.INSTANCE;
        FFloat32 float32 = FFloat32.INSTANCE;
        FFloat64 float64 = FFloat64.INSTANCE;
    }

    public enum SOME_TYPES_I_AM_TOO_TIRED_TO_NAME_AS_ENUM { //TODO either an enum or immutableset that contains all predef so checks are easy

    }

    public static final ImmutableSet<FClass> intTypes = ImmutableSet.of(
            FInt.INSTANCE,
            FInt32.INSTANCE,
            FInt64.INSTANCE
    );

    public static void load() {
        FBool.INSTANCE.addDefaultFunctions();
        FInt.INSTANCE.addDefaultFunctions();
        FInt32.INSTANCE.addDefaultFunctions();
        FInt64.INSTANCE.addDefaultFunctions();
        FFloat32.INSTANCE.addDefaultFunctions();
        FFloat64.INSTANCE.addDefaultFunctions();
    }

    public FPredefinedClass(FClassIdentifier identifier) {
        super(identifier, FVisibilityModifier.PUBLIC, false);
    }

    @Override
    public void addField(FField field) {
        throw new RuntimeException("somthing went terribly wrong");
    }

    @Override
    public void addFunction(FFunction function) {
        throw new RuntimeException("somthing went terribly wrong");
    }
}
