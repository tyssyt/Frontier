package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableSet;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;

public abstract class FPredefinedClass extends FClass {

    public static final ImmutableSet<FClass> intTypes =
            ImmutableSet.of(FInt.INSTANCE, FInt32.INSTANCE, FInt64.INSTANCE);

    public static final ImmutableSet<FClass> boolTypes = ImmutableSet.of(FBool.INSTANCE);

    public FPredefinedClass(FClassIdentifier identifier) {
        super(identifier, FVisibilityModifier.PUBLIC);
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
