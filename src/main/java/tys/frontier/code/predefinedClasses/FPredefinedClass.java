package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Utils;

public abstract class FPredefinedClass extends FBaseClass {

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
        namespace.addFunctionTrusted(new FunctionBuilder(UnaryOperator.NEG.identifier, namespace)
                .setVisibility(getVisibility()).setPredefined(true).setParams(this).setReturnType(this).build());

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
