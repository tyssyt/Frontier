package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.Collections;
import java.util.concurrent.ConcurrentMap;

public class FArray extends FPredefinedClass {

    public static final FIdentifier SIZE = new FIdentifier("size");
    public static final FIdentifier C_ARRAY = new FIdentifier("c_array");
    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;
    private Pair<FFunction, FFunction> access;
    private FField size;

    @Override
    public boolean canImplicitlyCast() {
        return false; //TODO remove once arrays use generics
    }

    private FArray(FType baseType) {
        super(new FArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        DefaultNamespace namespace = getNamespace();

        addDefaultFunctions();
        //TODO add container equals, and prolly do something to equality once that is done
        size = new FField(SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false, false);
        addFieldTrusted(size); //TODO make final
        access = Access.createPredefined(this, FIntN._32, baseType);
        namespace.addFunctionTrusted(access.a);
        namespace.addFunctionTrusted(access.b);
        namespace.addFunctionTrusted(FBaseFunction.createPredefined(C_ARRAY, namespace, FVisibilityModifier.EXPORT, CArray.getArrayFrom(baseType), ImmutableList.of(FParameter.create(FIdentifier.THIS, this, false)), null, Collections.emptyMap()));

        namespace.addFunctionTrusted(FConstructor.createPredefined(FVisibilityModifier.EXPORT, this));
        setForImpl(new ForByIdx(access.a, size.getGetter()));
    }

    public static FArray getArrayFrom(FType baseClass) {
        return existing.computeIfAbsent(baseClass, p -> new FArray(baseClass));
    }

    public FType getBaseType() {
        return baseType;
    }

    public FFunction getArrayGet() {
        return access.a;
    }

    public FFunction getArraySet() {
        return access.b;
    }

    public FField getSize() {
        return size;
    }

    @Override
    public long concreteness() { //TODO once array uses generics this is no longer necessary
        long res = baseType.concreteness();
        if (res == Long.MAX_VALUE) //avoid overflow
            return Long.MAX_VALUE;
        return res+1;
    }
}
