package tys.frontier.modules.io;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FFunction;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public class IOClass extends FPredefinedClass {

    public static final FTypeIdentifier IDENTIFIER = new FTypeIdentifier("IO");
    public static final FFunctionIdentifier PUTCHAR_ID = new FFunctionIdentifier("putchar");
    public static final FFunctionIdentifier GETCHAR_ID = new FFunctionIdentifier("getchar");

    public static final IOClass INSTANCE = new IOClass();

    private IOClass() {
        super(IDENTIFIER);
        try {
            addFunction(new FFunction(PUTCHAR_ID, this, FVisibilityModifier.EXPORT, true, FVoid.INSTANCE,
                    ImmutableList.of(
                            new FParameter(new FVariableIdentifier("char"), FIntN._32)
                    )){{predefined = true;}});
            addFunction(
                    new FFunction(GETCHAR_ID, this, FVisibilityModifier.EXPORT, true, FIntN._32, ImmutableList.of())
            {{predefined = true;}});
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }
}
