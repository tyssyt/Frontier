package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;
import tys.frontier.util.IntPair;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<IntPair<FClass>, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseClass;
    private int depth;

    private FArray(FClass baseClass, int depth) {
        super(new FArrayIdentifier(baseClass.getIdentifier()), FVisibilityModifier.PUBLIC);
        this.baseClass = baseClass;
        this.depth = depth;

        {
            //create constructors
            ImmutableList.Builder<FLocalVariable> builder = new ImmutableList.Builder<>();
            for (int i = 0; i < depth; i++) {
                builder.add(new FLocalVariable(new FVariableIdentifier("i" + i), FInt.INSTANCE));
                FConstructor c = new FConstructor(FVisibilityModifier.PUBLIC, this, builder.build());
                functions.put(FFunctionIdentifier.CONSTRUCTOR, c);
            }
        }
    }

    public static FArray getArrayFrom(FClass baseClass, int arrayDepth) {
        IntPair<FClass> pair = new IntPair<>(baseClass, arrayDepth);
        FArray old = existing.get(pair);
        if (old == null) {
            return existing.put(pair, new FArray(baseClass, arrayDepth));
        }
        return old;
    }

    public FClass getBaseClass() {
        return baseClass;
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
