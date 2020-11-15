package tys.frontier.code.predefinedClasses;

import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.identifier.FIdentifier;
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

        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT32).setReturnType(int32).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_INT64).setReturnType(int64).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TO_FLOAT32).setReturnType(FFloat32.INSTANCE).build());
    }
}
