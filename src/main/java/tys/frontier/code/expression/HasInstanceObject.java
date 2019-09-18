package tys.frontier.code.expression;

public interface HasInstanceObject extends FVariableExpression {

    HasInstanceObject copy();

    FExpression getObject();
    void setObject(FExpression object);

    default boolean isStatic() {
        return getObject() == null;
    }

}
