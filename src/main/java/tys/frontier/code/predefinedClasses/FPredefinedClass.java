package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Utils;

import static java.util.Collections.emptyMap;

public abstract class FPredefinedClass extends FBaseClass {

    public static FIdentifier SHIFT_L = new FIdentifier("shiftL");
    public static FIdentifier U_SHIFT_R = new FIdentifier("uShiftR");
    public static FIdentifier S_SHIFT_R = new FIdentifier("sShiftR");

    public FPredefinedClass(FIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT, false);
    }

    @Override
    public void removeUnreachable(Reachability.ReachableNamespace reachable) {}

    @Override
    public boolean isPredefined() {
        return true;
    }

    protected void addPredefinedFunctionsForArithType() {
        DefaultNamespace namespace = this.getNamespace();
        namespace.addFunctionTrusted(UnaryOperator.NEG.createPredefined(this, this));

        try {
            namespace.addRemoteFunction(BinaryOperator.EQUALS.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.NOT_EQUALS.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.EQUALS_ID.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.NOT_EQUALS_ID.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.LESS.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.LESS_EQUAL.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.GREATER.addPredefined(this, FBool.INSTANCE));
            namespace.addRemoteFunction(BinaryOperator.GREATER_EQUAL.addPredefined(this, FBool.INSTANCE));

            namespace.addRemoteFunction(BinaryOperator.PLUS.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.MINUS.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.TIMES.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.DIVIDED.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.MODULO.addPredefined(this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    protected void addPredefinedFunctionsForIntType() {
        DefaultNamespace namespace = this.getNamespace();

        try {
            namespace.addRemoteFunction(BinaryOperator.AAND.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.AOR.addPredefined(this, this));
            namespace.addRemoteFunction(BinaryOperator.XOR.addPredefined(this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }

        namespace.addFunctionTrusted(createPredefined(SHIFT_L));
        namespace.addFunctionTrusted(createPredefined(U_SHIFT_R));
        namespace.addFunctionTrusted(createPredefined(S_SHIFT_R));
    }

    private FFunction createPredefined(FIdentifier identifier) {
        ImmutableList<FParameter> params = ImmutableList.of(
                FParameter.create(new FIdentifier("first"), this, false),
                FParameter.create(new FIdentifier("second"), this, false)
        );
        return new FBaseFunction(identifier, getNamespace(), FVisibilityModifier.EXPORT, false, this, params, null, emptyMap()) {
            {predefined = true;}
        };
    }
}
