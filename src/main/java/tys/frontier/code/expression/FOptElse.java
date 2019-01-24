package tys.frontier.code.expression;

import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.typeInference.IsType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class FOptElse implements FExpression {

    private FExpression optional;
    private FExpression elze;

    public FOptElse(FExpression optional, FExpression elze) throws IncompatibleTypes {
        this.optional = optional;
        this.elze = elze;
        checkTypes();
    }

    public static FOptElse create(FExpression optional, FExpression elze) throws IncompatibleTypes {
        return new FOptElse(optional, elze);
    }

    private void checkTypes() throws IncompatibleTypes { //TODO the inner type of the optional could be allowed to implicitly cast to type of else I guess?
        if (optional instanceof FTypeVariable) {
            ((FTypeVariable) optional).addConstraint(new IsType(this, FOptional.from(elze.getType())));
            return;
        }

        if (!(optional.getType() instanceof FOptional)) {
            throw new IncompatibleTypes(optional.getType(), FOptional.from(optional.getType()));
        }
        elze = elze.typeCheck(((FOptional) optional.getType()).getBaseType());
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

    @Override
    public String toString() {
        return tS();
    }
}
