package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.style.order.Alphabetical;

import java.util.ArrayList;
import java.util.List;

public class FConstructor extends FFunction {

    public static final FFunctionIdentifier IDENTIFIER = new FFunctionIdentifier("!new");


    private FConstructor(FVisibilityModifier modifier, FClass fClass, ImmutableList<FParameter> params) {
        super(IDENTIFIER, fClass, modifier, false, true, fClass, params);
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

    @Override
    public MemberType getMemberType() {
        return MemberType.CONSTRUCTOR;
    }

    @Override
    public boolean isConstructor() {
        return true;
    }

    private static ImmutableList<FParameter> getParameters(FClass fClass) {
        return fClass.getInstanceFields().values().stream()
                .sorted(Alphabetical.INSTANCE) //TODO alphabetical order is far from a good choice here, but for now...
                .map(field -> new FParameter(field.getIdentifier(), field.getType(), field.getType().getDefaultValue()))
                .collect(ImmutableList.toImmutableList());
    }

    private void generateBody() {
        List<FStatement> statements = new ArrayList<>();
        for (FParameter param : getParams()) {
            FExpression thisExpr = new FLocalVariableExpression(getMemberOf().getThis());
            FField field = getMemberOf().getInstanceFields().get(param.getIdentifier());
            statements.add(FVarAssignment.createTrusted(FFieldAccess.createInstanceTrusted(field, thisExpr), FVarAssignment.Operator.ASSIGN, new FLocalVariableExpression(param)));
        }
        statements.add(FReturn.createTrusted(new FLocalVariableExpression(getMemberOf().getThis()), this));
        setBody(FBlock.from(statements));
    }
}
