package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.Operator.FOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.style.order.Alphabetical;

public class FConstructor extends FFunction {

    private FConstructor(FVisibilityModifier modifier, FClass fClass, ImmutableList<FParameter> params) {
        super(FOperator.CONSTRUCTOR, fClass, modifier, true, fClass, params);
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.CONSTRUCTOR;
    }

    @Override
    public boolean isConstructor() {
        return true;
    }

    public static FConstructor create(FVisibilityModifier modifier, FClass fClass) {
        FConstructor res = new FConstructor(modifier, fClass, getParameters(fClass));
        res.generateBody();
        return res;
    }

    public static FConstructor createPredefined(FVisibilityModifier modifier, FClass fClass) {
        FConstructor res = new FConstructor(modifier, fClass, getParameters(fClass));
        res.predefined = true;
        return res;
    }

    private static ImmutableList<FParameter> getParameters(FClass fClass) {
        return fClass.getFields().values().stream()
                .filter(field -> !field.isStatic())
                .sorted(Alphabetical.INSTANCE) //TODO alphabetical order is far from a good choice here, but for now...
                .map(field -> new FParameter(field.getIdentifier(), field.getType(), field.getType().getDefaultValue()))
                .collect(ImmutableList.toImmutableList());
    }

    private void generateBody() {
        ImmutableList.Builder<FStatement> statements = ImmutableList.builder();
        for (FParameter param : getParams()) {
            FExpression thisExpr = new FLocalVariableExpression(getClazz().getThis());
            FField field = getClazz().getField(param.getIdentifier());
            statements.add(new FVarAssignment(new FFieldAccess(field, thisExpr), FVarAssignment.Operator.ASSIGN, new FLocalVariableExpression(param)));
        }
        statements.add(new FReturn(new FLocalVariableExpression(getClazz().getThis()), this));
        setBody(statements.build());
    }
}
