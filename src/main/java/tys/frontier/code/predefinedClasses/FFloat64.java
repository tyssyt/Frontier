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

        DefaultNamespace namespace = this.getNamespace();

        FunctionBuilder builder = new FunctionBuilder().setMemberOf(getNamespace()).setPredefined(true).setParams(this);
        FBaseFunction rawBits = builder.setIdentifier(RAW_BITS).setReturnType(FIntN._32).build();
        namespace.addFunctionTrusted(rawBits);
        builder.setReturnType(this);
        namespace.addFunctionTrusted(builder.setIdentifier(LOG).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG10).build());
        namespace.addFunctionTrusted(builder.setIdentifier(LOG2).build());
        namespace.addFunctionTrusted(builder.setIdentifier(CEIL).build());
        namespace.addFunctionTrusted(builder.setIdentifier(FLOOR).build());
        namespace.addFunctionTrusted(builder.setIdentifier(TRUNC).build());
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }

    @Override
    public int getBits() {
        return 64;
    }
}
