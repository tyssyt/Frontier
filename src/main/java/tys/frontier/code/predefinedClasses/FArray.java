package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.concurrent.ConcurrentMap;

public class FArray extends FPredefinedClass {

    public static final AttributeIdentifier SIZE = new AttributeIdentifier("size");
    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;

    @Override
    public boolean canImplicitlyCast() {
        return false; //TODO remove once arrays use generics
    }

    private FArray(FType baseType) {
        super(new FArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        addDefaultFunctions();
        //TODO add container equals, and prolly do something to equality once that is done
        FField size = new FField(SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false, false);
        addFieldTrusted(size); //TODO make final
        addFunctionTrusted(FConstructor.createPredefined(FVisibilityModifier.EXPORT, this));
        Pair<FFunction, FFunction> access = Access.createPredefined(this, FIntN._32, baseType);
        addFunctionTrusted(access.a);
        addFunctionTrusted(access.b);
        setForImpl(new ForByIdx(access.a, size.getGetter()));
    }

    public static FArray getArrayFrom(FType baseClass) {
        return existing.computeIfAbsent(baseClass, p -> new FArray(baseClass));
    }

    public FType getBaseType() {
        return baseType;
    }

    @Override
    public long concreteness() { //TODO once array uses generics this is no longer necessary
        long res = baseType.concreteness();
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }
}
