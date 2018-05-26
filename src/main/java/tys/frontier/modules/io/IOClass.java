package tys.frontier.modules.io;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public class IOClass extends FPredefinedClass {

    public static final FClassIdentifier IDENTIFIER = new FClassIdentifier("IO");
    public static final FFunctionIdentifier PUTCHAR_ID = new FFunctionIdentifier("putchar");

    public static final IOClass INSTANCE = new IOClass();

    private IOClass() {
        super(IDENTIFIER);
        try {
            addFunction(new FFunction(PUTCHAR_ID, this, FVisibilityModifier.EXPORT, true, FVoid.INSTANCE,
                    ImmutableList.of(
                            new FParameter(new FVariableIdentifier("char"), FIntN._32)
                    )){{predefined = true;}});
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }
}
