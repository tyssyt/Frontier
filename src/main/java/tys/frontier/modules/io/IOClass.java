package tys.frontier.modules.io;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FInt32;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FVoid;

public class IOClass extends FPredefinedClass {

    public static final FClassIdentifier IDENTIFIER = new FClassIdentifier("IO");
    public static final FFunctionIdentifier PUTCHAR_ID = new FFunctionIdentifier("putchar");

    public static final IOClass INSTANCE = new IOClass();

    private IOClass() {
        super(IDENTIFIER);
        addFunctionInternal(new FFunction(PUTCHAR_ID, this, FVisibilityModifier.PUBLIC, true, FVoid.INSTANCE,
                ImmutableList.of(
                        new FLocalVariable(new FVariableIdentifier("char"), FInt32.INSTANCE)
                )){{predefined = true;}});
    }
}
