package tys.frontier.code.predefinedClasses;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.literal.FFloat32Literal;

public class FFloat32 extends FPredefinedClass {

    public static final FFloat32 INSTANCE = new FFloat32();

    private FFloat32 () {
        super(FTypeIdentifier.FLOAT32);
    }

    @Override
    public FExpression getDefaultValue() {
        return new FLiteralExpression(new FFloat32Literal(0, "0.0f"));
    }
}
