package tys.frontier.code.predefinedClasses;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.literal.FFloat64Literal;

public class FFloat64 extends FPredefinedClass {

    public static final FFloat64 INSTANCE = new FFloat64();

    private FFloat64 () {
        super(FClassIdentifier.FLOAT64);
    }

    @Override
    public FExpression getDefaultValue() {
        return new FLiteralExpression(new FFloat64Literal(0, "0.0d"));
    }
}
