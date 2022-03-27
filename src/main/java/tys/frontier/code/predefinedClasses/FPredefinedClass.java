package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FIf;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Utils;

import java.util.List;

import static java.util.Arrays.asList;

public abstract class FPredefinedClass extends FBaseClass {
    //TODO in theory, these should be dependent on the keywords in the style
    public static FIdentifier TO_CHAR = new FIdentifier("toChar");
    public static FIdentifier TO_INT32 = new FIdentifier("toInt32");
    public static FIdentifier TO_INT64 = new FIdentifier("toInt64");
    public static FIdentifier TO_FLOAT32 = new FIdentifier("toFloat32");
    public static FIdentifier TO_FLOAT64 = new FIdentifier("toFloat64");

    public static final FIdentifier MAX = new FIdentifier("max");
    public static final FIdentifier MIN = new FIdentifier("min");
    public static final FIdentifier CLAMP = new FIdentifier("clamp");

    //TODO @PositionForGeneratedCode
    public FPredefinedClass(FIdentifier identifier) {
        super(null, identifier, FVisibilityModifier.EXPORT, null);
    }

    @Override
    public boolean isPredefined() {
        return true;
    }

    protected void addPredefinedFunctionsForArithType() {
        addDefaultFunctions();
        DefaultNamespace namespace = this.getNamespace();
        FunctionBuilder builder = new FunctionBuilder(UnaryOperator.NEG.identifier, namespace).setReturnType(this)
                .setVisibility(FVisibilityModifier.EXPORT);
        builder.setPredefined(true).setParams(this);
        namespace.addFunctionTrusted(builder.build());

        try {
            BinaryOperator.EQUALS.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.NOT_EQUALS.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.LESS.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.LESS_EQUAL.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.GREATER.addPredefined(this, FBool.INSTANCE);
            BinaryOperator.GREATER_EQUAL.addPredefined(this, FBool.INSTANCE);

            BinaryOperator.PLUS.addPredefined(this, this);
            BinaryOperator.MINUS.addPredefined(this, this);
            BinaryOperator.TIMES.addPredefined(this, this);
            BinaryOperator.DIVIDED.addPredefined(this, this);
            BinaryOperator.MODULO.addPredefined(this, this);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }

        builder.setPredefined(false).setParams(this, this);
        FBaseFunction min = builder.setIdentifier(MIN).build();
        namespace.addFunctionTrusted(min);
        min.setBody(FBlock.from(buildChoice(min, BinaryOperator.LESS)));

        FBaseFunction max = builder.setIdentifier(MAX).build();
        namespace.addFunctionTrusted(max);
        max.setBody(FBlock.from(buildChoice(max, BinaryOperator.GREATER)));

        {
            FBaseFunction clamp = builder.setIdentifier(CLAMP).setParams(this, this, this).build();
            namespace.addFunctionTrusted(clamp);
            FParameter firstParam = clamp.getSignature().getParameters().get(0);
            FParameter thirdParam = clamp.getSignature().getParameters().get(2);

            Signature less = BinaryOperator.LESS.getFunctionTrusted(this, this);
            List<FExpression> arguments = asList(new FVariableExpression(null, firstParam), (new FVariableExpression(null, thirdParam)));
            FExpression lessCall = FFunctionCall.create(null, less, arguments);
            FIf then = buildChoice(clamp, BinaryOperator.GREATER);
            FReturn _else = FReturn.createTrusted(null, new FVariableExpression(null, thirdParam), clamp);
            clamp.setBody(FBlock.from(FIf.createTrusted(null, lessCall, FBlock.from(then), FBlock.from(_else))));
        }

    }

    private FIf buildChoice(FBaseFunction function, BinaryOperator op) {
        FParameter firstParam = function.getSignature().getParameters().get(0);
        FParameter secondParam = function.getSignature().getParameters().get(1);
        Signature sig = op.getFunctionTrusted(this, this);
        List<FExpression> arguments = asList(new FVariableExpression(null, firstParam), (new FVariableExpression(null, secondParam)));
        FExpression sigCall = FFunctionCall.create(null, sig, arguments);
        FReturn then = FReturn.createTrusted(null, new FVariableExpression(null, firstParam), function);
        FReturn _else = FReturn.createTrusted(null, new FVariableExpression(null, secondParam), function);
        return FIf.createTrusted(null, sigCall, FBlock.from(then), FBlock.from(_else));
    }
}
