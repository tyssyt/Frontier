package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarAssignment;

import java.util.ArrayList;
import java.util.Comparator;
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

    private static ImmutableList<FParameter> getParameters(FType fClass) {
        List<FParameter> arguments = new ArrayList<>();
        List<FParameter> defaultArguments = new ArrayList<>();

        for (FField field : fClass.getInstanceFields().values()) {
            if (field.getType() instanceof FOptional) {
                FExpression defaultValue = new FLiteralExpression(new FNull((FOptional) field.getType()));
                arguments.add(FParameter.createTrusted(field.getIdentifier(), field.getType(), defaultValue));
            } else {
                arguments.add(FParameter.create(field.getIdentifier(), field.getType(), false));
            }
        }

        arguments.sort(Comparator.comparing(o -> o.getIdentifier().name)); //TODO alphabetical order is far from a good choice here, but for now...
        defaultArguments.sort(Comparator.comparing(o -> o.getIdentifier().name));
        return new ImmutableList.Builder<FParameter>().addAll(arguments).addAll(defaultArguments).build();
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
