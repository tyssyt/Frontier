package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.StaticField;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FFloat64Literal;
import tys.frontier.code.namespace.DefaultNamespace;

public class FFloat64 extends FFloat {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {
        super(FIdentifier.FLOAT64);
        addPredefinedFunctionsForArithType();
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }

    @Override
    public int getBits() {
        return 64;
    }

    @SuppressWarnings("SameParameterValue")
    void addIntFunctions(FIntN int32, FIntN int64) { //needed to avoid cyclic class loader dependencies
        DefaultNamespace namespace = this.getNamespace();

        {
            StaticField nan = new StaticField(null, NAN, this, namespace, FVisibilityModifier.EXPORT, true);
            nan.setAssignmentTrusted(new FLiteralExpression(null, new FFloat64Literal(Double.NaN, NAN.name)));
            namespace.addFieldTrusted(nan);

            StaticField infPos = new StaticField(null, INF_POS, this, namespace, FVisibilityModifier.EXPORT, true);
            infPos.setAssignmentTrusted(new FLiteralExpression(null, new FFloat64Literal(Double.POSITIVE_INFINITY, INF_POS.name)));
            namespace.addFieldTrusted(infPos);

            StaticField infNeg = new StaticField(null, INF_NEG, this, namespace, FVisibilityModifier.EXPORT, true);
            infNeg.setAssignmentTrusted(new FLiteralExpression(null, new FFloat64Literal(Double.NEGATIVE_INFINITY, INF_NEG.name)));
            namespace.addFieldTrusted(infNeg);
        }

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(getNamespace()).setPredefined(true).setParams(this);
        FBaseFunction rawBits = builder.setIdentifier(RAW_BITS).setReturnType(int32).build();
        namespace.addFunctionTrusted(rawBits);
        builder.setReturnType(this);

        namespace.addFunctionTrusted(builder.setIdentifier(LOG).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG10).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG2).build());
        namespace.addFunctionTrusted(builder.setIdentifier(CEIL).build());
        namespace.addFunctionTrusted(builder.setIdentifier(FLOOR).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TRUNC).build());
        namespace.addFunctionTrusted(builder.setIdentifier(SIN).build());
        namespace.addFunctionTrusted(builder.setIdentifier(COS).build());

        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT32).setReturnType(int32).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT64).setReturnType(int64).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_FLOAT32).setReturnType(FFloat32.INSTANCE).build());
    }
}
