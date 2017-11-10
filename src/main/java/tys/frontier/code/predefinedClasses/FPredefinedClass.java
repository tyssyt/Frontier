package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;

public abstract class FPredefinedClass extends FClass {

    static {
        //make sure all important classes are loaded
        FBool bool = FBool.INSTANCE;
        FInt fInt = FInt.INSTANCE;
        FInt32 int32 = FInt32.INSTANCE;
        FInt64 int64 = FInt64.INSTANCE;
        FFloat32 float32 = FFloat32.INSTANCE;
        FFloat64 float64 = FFloat64.INSTANCE;
    }

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
    public void addField(FField field) throws IdentifierCollision {
        throw new RuntimeException("somthing went terribly wrong");
    }

    @Override
    public void addFunction(FFunction function) throws SignatureCollision {
        throw new RuntimeException("somthing went terribly wrong");
    }
}
