package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FClass, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseClass;

    private FArray(FClass baseClass) {
        super(new FArrayIdentifier(baseClass.getIdentifier()), FVisibilityModifier.PUBLIC);
        this.baseClass = baseClass;
    }

    public static FArray getArrayFrom(FClass baseClass) {
        return existing.putIfAbsent(baseClass, new FArray(baseClass));
    }

    public static FArray getMultiArrayFrom(FClass baseClass, int arrayDepth) {
        FArray cur = getArrayFrom(baseClass);
        for (int i=1; i<arrayDepth; i++) {
            cur = getArrayFrom(cur);
        }
        return cur;
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
