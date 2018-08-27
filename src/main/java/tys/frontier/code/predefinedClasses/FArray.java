package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FConstructor;
import tys.frontier.code.FField;
import tys.frontier.code.FType;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.InterfaceInstanceField;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.IntPair;
import tys.frontier.util.Utils;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FPredefinedClass {

    public static final FVariableIdentifier SIZE = new FVariableIdentifier("size");
    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<IntPair<FType>, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;
    private int depth;

    private FArray(FType baseType, int depth) {
        super(new FArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        this.depth = depth;
        addDefaultFunctions();
        //TODO add container equals, and prolly do something to equality once that is done
        try {
            addField(new FField(SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false)); //TODO make final
        } catch (IdentifierCollision | InterfaceInstanceField e) {
            Utils.handleException(e);
        }
        //create constructor
        try {
            addFunction(FConstructor.createPredefined(FVisibilityModifier.EXPORT, this));
        } catch (SignatureCollision e) {
            Utils.handleException(e);
        }
    }

    public static FArray getArrayFrom(FType baseClass, int arrayDepth) {
        assert arrayDepth > 0;
        IntPair<FType> pair = new IntPair<>(baseClass, arrayDepth);
        return existing.computeIfAbsent(pair, p -> new FArray(baseClass, arrayDepth));
    }

    public FType getBaseType() {
        return baseType;
    }

    public int getDepth() {
        return depth;
    }

    public FType getOneDimensionLess() {
        if (depth == 1)
            return baseType;
        return getArrayFrom(baseType, depth-1);
    }

}
