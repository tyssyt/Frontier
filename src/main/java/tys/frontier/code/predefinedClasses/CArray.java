package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.identifier.CArrayIdentifier;
import tys.frontier.code.type.FType;

import java.util.concurrent.ConcurrentMap;

public class CArray extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, CArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }

    private CArray(FType baseType) {
        super(new CArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        addDefaultFunctions();
    }

    public static CArray getArrayFrom(FType baseType) {
        return existing.computeIfAbsent(baseType, p -> new CArray(baseType));
    }

    public FType getBaseType() {
        return baseType;
    }

    @Override
    public long concreteness() {
        long res = baseType.concreteness();
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }
}
