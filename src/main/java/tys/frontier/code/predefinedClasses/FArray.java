package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.Set;
import java.util.concurrent.ConcurrentMap;

import static java.util.Collections.emptySet;
import static tys.frontier.util.Utils.mutableSingletonList;

public class FArray extends FPredefinedClass {

    public static final FIdentifier SIZE = new FIdentifier("size");
    public static final FIdentifier C_ARRAY = new FIdentifier("c_array");
    public static final FIdentifier COPY = new FIdentifier("copy");
    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FArray> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;
    private Pair<FFunction, FFunction> access;
    private InstanceField size;

    @Override
    public boolean canImplicitlyCast() {
        return false; //TODO remove once arrays use generics
    }

    //TODO @PositionForGeneratedCode
    private FArray(FType baseType) {
        super(new FArrayIdentifier(baseType.getIdentifier()));
        this.baseType = baseType;
        DefaultNamespace namespace = getNamespace();

        //addDefaultFunctions(); TODO should only be added once for "base class"
        //TODO add container equals, and prolly do something to equality once that is done
        size = new InstanceField(null, SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false);
        addFieldTrusted(size); //TODO make final
        namespace.getFunctions(true).get(size.getIdentifier()).clear(); //remove setter TODO no longer needed when field is final
        access = Access.createPredefined(this, baseType, FIntN._32);
        namespace.addFunctionTrusted(access.a);
        namespace.addFunctionTrusted(access.b);

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(namespace).setPredefined(true);

        builder.setIdentifier(C_ARRAY);
        namespace.addFunctionTrusted(builder.setParams(this).setReturnType(CArray.getArrayFrom(this)).build());

        builder.setIdentifier(COPY);
        ImmutableList<FParameter> params = ImmutableList.of(
                FParameter.create(new FIdentifier("source"), this, false),
                FParameter.create(new FIdentifier("target"), this, false),
                FParameter.create(new FIdentifier("sourceOffset"), FIntN._32, true),
                FParameter.create(new FIdentifier("targetOffset"), FIntN._32, true),
                FParameter.create(new FIdentifier("length"), FIntN._32, true)
        );
        params.get(2).setDefaultValueTrusted(new FLiteralExpression(null, new FIntNLiteral(0)), emptySet());
        params.get(3).setDefaultValueTrusted(new FLiteralExpression(null, new FIntNLiteral(0)), emptySet());
        FFunctionCall sourceSize = FFunctionCall.createTrusted(null, size.getGetter().getSignature(), mutableSingletonList(new FVariableExpression(null, params.get(0))));
        params.get(4).setDefaultValueTrusted(sourceSize, Set.of(params.get(0)));
        namespace.addFunctionTrusted(builder.setParams(params).setReturnType(FTuple.VOID).build());

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
    public int concreteness() { //TODO once array uses generics this is no longer necessary
        int res = baseType.concreteness();
        if (res == Integer.MAX_VALUE) //avoid overflow
            return Integer.MAX_VALUE;
        return res+1;
    }
}
