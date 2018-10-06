package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FClass;
import tys.frontier.code.identifier.FOptionalIdentifier;

import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FClass, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FClass baseType;

    private FOptional(FClass baseType) {
        super(new FOptionalIdentifier(baseType.getIdentifier()));
        assert !(baseType instanceof FOptional);
        this.baseType = baseType;
        addDefaultFunctions();

        //TODO add functions/fields delegating to underlying class
    }

    public static FOptional from(FClass baseClass) {
        return existing.computeIfAbsent(baseClass, p -> new FOptional(baseClass));
    }

    public FClass getBaseType() {
        return baseType;
    }
}
