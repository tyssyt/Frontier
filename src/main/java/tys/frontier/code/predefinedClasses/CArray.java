package tys.frontier.code.predefinedClasses;

import com.google.common.collect.MapMaker;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.CArrayIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.concurrent.ConcurrentMap;

public class CArray extends FPredefinedClass {

    //TODO once we have fancy iterators, I want to be able to iterate over a cArray, where I obv need to specify the size before doing so

    public static final FIdentifier OF = new FIdentifier("of");
    public static final FIdentifier COPY_TO_F_ARRAY = new FIdentifier("copyToFArray");


    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FArray, CArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }

    private CArray(FArray fArray) {
        super(new CArrayIdentifier(fArray.getBaseType().getIdentifier()));
        baseType = fArray.getBaseType();
        DefaultNamespace namespace = getNamespace();
        //addDefaultFunctions(); TODO should only be added once for "base class"

        Pair<FFunction, FFunction> access = Access.createPredefined(this, baseType, FIntN._32, FIntN._32);
        namespace.addFunctionTrusted(access.a);
        namespace.addFunctionTrusted(access.b);

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(namespace).setPredefined(true);

        builder.setIdentifier(OF);
        namespace.addFunctionTrusted(builder.setParams(baseType).setReturnType(this).build());

        builder.setIdentifier(COPY_TO_F_ARRAY);
        namespace.addFunctionTrusted(builder.setParams(this, FIntN._32).setReturnType(fArray).build());
    }

    public static CArray getArrayFrom(FArray fArray) {
        return existing.computeIfAbsent(fArray, p -> new CArray(fArray));
    }

    public FType getBaseType() {
        return baseType;
    }

    @Override
    public int concreteness() {
        int res = baseType.concreteness();
        if (res == Integer.MAX_VALUE) //avoid overflow
            return Integer.MAX_VALUE;
        return res+1;
    }
}
