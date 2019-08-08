package tys.frontier.code.expression;

import tys.frontier.code.Typed;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.StringBuilderToString;

public interface FExpression extends Typed, StringBuilderToString {

    <E> E accept(ExpressionVisitor<E> visitor);
    <E> E accept(ExpressionWalker<E> walker);

    default FExpression typeCheck(FType targetType) throws IncompatibleTypes {
        return FImplicitCast.create(targetType, this, Variance.Covariant);
    }


}
