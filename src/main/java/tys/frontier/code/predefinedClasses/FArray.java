package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FClass;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FPredefinedClass {

    public static final FVariableIdentifier SIZE = new FVariableIdentifier("size");
    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FClass, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseType;

    private FArray(FClass baseType) {
        super(new FArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        addDefaultFunctions();
        //TODO add container equals, and prolly do something to equality once that is done
        try {
            addField(new FField(SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false)); //TODO make final
        } catch (IdentifierCollision identifierCollision) {
            Utils.handleException(identifierCollision);
        }
        //create constructor
        try {
            addFunction(FConstructor.createPredefined(FVisibilityModifier.EXPORT, this));
        } catch (SignatureCollision e) {
            Utils.handleException(e);
        }
    }

    public static FArray getArrayFrom(FClass baseClass) {
        return existing.computeIfAbsent(baseClass, p -> new FArray(baseClass));
    }

    public FClass getBaseType() {
        return baseType;
    }
}
