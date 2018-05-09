package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.IntPair;
import tys.frontier.util.Utils;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<IntPair<FClass>, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseClass;
    private int depth;

    private FArray(FClass baseClass, int depth) {
        super(new FArrayIdentifier(baseClass.getIdentifier()));
        this.baseClass = baseClass;
        this.depth = depth;
        addDefaultFunctions();
        //TODO add container equals, and prolly do something to equality once that is done
        try {
            addField(new FField(FVariableIdentifier.SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false)); //TODO make final
        } catch (IdentifierCollision identifierCollision) {
            Utils.handleException(identifierCollision);
        }
        {
            //create constructors
            ImmutableList.Builder<FLocalVariable> builder = new ImmutableList.Builder<>();
            for (int i = 0; i < depth; i++) {
                builder.add(new FLocalVariable(new FVariableIdentifier("i" + i), FIntN._32)); //TODO other int types, solved when we have promotion
                try {
                    addFunction(new FConstructor(FVisibilityModifier.EXPORT, this, builder.build()){{predefined=true;}});
                } catch (SignatureCollision e) {
                    Utils.handleException(e);
                }
            }

        }
    }

    public static FArray getArrayFrom(FClass baseClass, int arrayDepth) {
        assert arrayDepth > 0;
        IntPair<FClass> pair = new IntPair<>(baseClass, arrayDepth);
        return existing.computeIfAbsent(pair, p -> new FArray(baseClass, arrayDepth));
    }

    public FClass getBaseClass() {
        return baseClass;
    }

    public int getDepth() {
        return depth;
    }

    public FClass getOneDimensionLess() {
        if (depth == 1)
            return baseClass;
        return getArrayFrom(baseClass, depth-1);
    }

}
