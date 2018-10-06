package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.predefinedClasses.FIntN;

public class IntLiteralTooLarge extends IncompatibleTypes {

    public final FIntNLiteral literal;

    public IntLiteralTooLarge(FIntNLiteral literal, FIntN targetType) {
        super(literal + " can't fit in " + targetType.getN() + " bits", targetType, literal.getType());
        this.literal = literal;
    }
}
