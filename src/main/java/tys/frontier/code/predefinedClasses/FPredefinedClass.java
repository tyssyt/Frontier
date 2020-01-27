package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Utils;

public abstract class FPredefinedClass extends FBaseClass {

    public FPredefinedClass(FIdentifier identifier) {
        super(identifier, FVisibilityModifier.EXPORT, false);
    }

    @Override
    public void removeUnreachable(Reachability.ReachableClass reachable) {}

    protected void addPredefinedFunctionsForArithType() {
        try {
            addFunction(UnaryOperator.NEG.createPredefined(this, this));

            addFunction(BinaryOperator.EQUALS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.NOT_EQUALS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.NOT_EQUALS_ID.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.LESS.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.LESS_EQUAL.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.GREATER.createPredefined(this, this, FBool.INSTANCE));
            addFunction(BinaryOperator.GREATER_EQUAL.createPredefined(this, this, FBool.INSTANCE));

            addFunction(BinaryOperator.PLUS.createPredefined(this, this, this));
            addFunction(BinaryOperator.MINUS.createPredefined(this, this, this));
            addFunction(BinaryOperator.TIMES.createPredefined(this, this, this));
            addFunction(BinaryOperator.DIVIDED.createPredefined(this, this, this));
            addFunction(BinaryOperator.MODULO.createPredefined(this, this, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }
}
