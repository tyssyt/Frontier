package tys.frontier.code.expression;

import tys.frontier.code.FType;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.expression.cast.TypeConversion;
import tys.frontier.code.literal.FLiteral;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FLiteralExpression implements FExpression {

    private FLiteral literal;

    public FLiteralExpression(FLiteral literal) {
        this.literal = literal;
    }

    public FLiteral getLiteral() {
        return literal;
    }

    @Override
    public FType getType() {
        return literal.getType();
    }

    @Override
    public FExpression typeCheck(FType targetType) throws IncompatibleTypes {
        if (this.getType() == targetType)
            return this;
        if (literal == FNull.UNTYPED && targetType instanceof FOptional) { //handle null literals here becasue they are special
            return new FLiteralExpression(new FNull((FOptional) targetType));
        }
        FType specifyTarget;
        if (targetType instanceof FOptional)
            specifyTarget = ((FOptional) targetType).getBaseType();
        else
            specifyTarget = targetType;
        FExpression res = new FLiteralExpression(literal.specify(specifyTarget)); //handle all other literals types here
        if (res.getType() == targetType)
            return res;
        FExpression casted = FImplicitCast.create(targetType, res, Variance.Covariant);
        //specify should handle all casts but the to optional
        assert casted instanceof FImplicitCast && ((FImplicitCast) casted).getTypeCast() instanceof TypeConversion && ((TypeConversion) ((FImplicitCast) casted).getTypeCast()).getCastType() == TypeConversion.CastType.TO_OPTIONAL;
        return casted;
    }

    public int distance(FLiteralExpression other) {
        return literal.distance(other.literal);
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitLiteral(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitLiteral(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(literal);
    }
    @Override
    public String toString() {
        return tS();
    }
}
