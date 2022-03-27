package tys.frontier.code.expression;

import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FOptElse extends FExpression {

    private FExpression optional;
    private FExpression elze;

    private FOptElse(Position position, FExpression optional, FExpression elze) throws IncompatibleTypes {
        super(position);
        this.optional = optional;
        this.elze = elze;
        checkTypes();
    }

    public static FOptElse create(Position position, FExpression optional, FExpression elze) throws IncompatibleTypes {
        return new FOptElse(position, optional, elze);
    }

    public static FOptElse createTrusted(Position position, FExpression optional, FExpression elze)  {
        try {
            return create(position, optional, elze);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    private void checkTypes() throws IncompatibleTypes { //TODO the inner type of the optional could be allowed to implicitly cast to type of else I guess?
        FType optType = optional.getType();
        if (optType instanceof FTypeVariable) {
            FTypeVariable var = (FTypeVariable) optType;
            FClass opt = FOptional.from(elze.getType());
            if (!var.satisfies(new ImplicitCastable(this, opt, Variance.Invariant)))
                throw new IncompatibleTypes(getPosition(), var, opt);
            return;
        }

        if (!FOptional.canBeTreatedAsOptional(optType)) {
            throw new IncompatibleTypes(getPosition(), optType, FOptional.from(optType));
        }
        elze = elze.typeCheck(optType instanceof FOptional ? ((FOptional) optType).getBaseType() : optType);
    }

    public FExpression getOptional() {
        return optional;
    }

    public FExpression getElse() {
        return elze;
    }

    @Override
    public FType getType() {
        return elze.getType();
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterOptElse(this);
        return visitor.exitOptElse(this, optional.accept(visitor), elze.accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitOptElse(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(optional).append(" : ").append(elze);
    }
}
