package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

public abstract class FPredefinedClass extends FBaseClass {
    //TODO in theory, these should be dependent on the keywords in the style
    public static FIdentifier TO_CHAR = new FIdentifier("toChar");
    public static FIdentifier TO_INT32 = new FIdentifier("toInt32");
    public static FIdentifier TO_INT64 = new FIdentifier("toInt64");
    public static FIdentifier TO_FLOAT32 = new FIdentifier("toFloat32");
    public static FIdentifier TO_FLOAT64 = new FIdentifier("toFloat64");

    //TODO @PositionForGeneratedCode
    public FPredefinedClass(FIdentifier identifier) {
        super(null, identifier, FVisibilityModifier.EXPORT, null);
    }

    @Override
    public boolean isPredefined() {
        return true;
    }

    protected void addPredefinedFunctionsForArithType() {
        DefaultNamespace namespace = this.getNamespace();
        namespace.addFunctionTrusted(new FunctionBuilder(UnaryOperator.NEG.identifier, namespace)
                .setVisibility(FVisibilityModifier.EXPORT).setPredefined(true).setParams(this).setReturnType(this).build());

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
}
