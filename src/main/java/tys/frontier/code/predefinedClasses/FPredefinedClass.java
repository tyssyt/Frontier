package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.passes.analysis.reachability.Reachability;

import static java.util.Collections.emptyMap;

public abstract class FPredefinedClass extends FBaseClass {

    public static FIdentifier SHIFT_L = new FIdentifier("shiftL");
    public static FIdentifier U_SHIFT_R = new FIdentifier("uShiftR");
    public static FIdentifier S_SHIFT_R = new FIdentifier("sShiftR");

    public FPredefinedClass(FIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT, false);
    }

    @Override
    public void removeUnreachable(Reachability.ReachableClass reachable) {}

    protected void addPredefinedFunctionsForArithType() {
        addFunctionTrusted(UnaryOperator.NEG.createPredefined(this, this));

        addFunctionTrusted(BinaryOperator.EQUALS.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.NOT_EQUALS.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.LESS.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.LESS_EQUAL.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.GREATER.createPredefined(this, this, FBool.INSTANCE));
        addFunctionTrusted(BinaryOperator.GREATER_EQUAL.createPredefined(this, this, FBool.INSTANCE));

        addFunctionTrusted(BinaryOperator.PLUS.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.MINUS.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.TIMES.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.DIVIDED.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.MODULO.createPredefined(this, this, this));
    }

    protected void addPredefinedFunctionsForIntType() {
        addFunctionTrusted(BinaryOperator.AAND.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.AOR.createPredefined(this, this, this));
        addFunctionTrusted(BinaryOperator.XOR.createPredefined(this, this, this));
        addFunctionTrusted(createPredefined(SHIFT_L));
        addFunctionTrusted(createPredefined(U_SHIFT_R));
        addFunctionTrusted(createPredefined(S_SHIFT_R));
    }

    private FFunction createPredefined(FIdentifier identifier) {
        ImmutableList<FParameter> params = ImmutableList.of(
                FParameter.create(new FIdentifier("first"), this, false),
                FParameter.create(new FIdentifier("second"), this, false)
        );
        return new FBaseFunction(identifier, this, FVisibilityModifier.EXPORT, false, this, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}
