package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FClass;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static tys.frontier.util.Utils.mutableSingletonList;

public class FConstructor extends FBaseFunction {

    public static final FIdentifier NEW_ID = new FIdentifier("!new");
    public static final FIdentifier MALLOC_ID = new FIdentifier("!malloc");

    private FConstructor(FVisibilityModifier modifier, FClass fClass, ImmutableList<FParameter> params) {
        super(fClass.getNamespace().getLocation(), NEW_ID, fClass.getNamespace(), modifier, null, false, fClass, params, null, emptyMap());
    }

    @Override
    public DefaultNamespace getMemberOf() {
        return (DefaultNamespace) super.getMemberOf();
    }

    public static FFunction createMalloc(FClass fClass) {
        FBaseFunction function = new FunctionBuilder(MALLOC_ID, fClass.getNamespace()).setVisibility(FVisibilityModifier.PRIVATE).setReturnType(fClass).build();
        function.predefined = true;
        return function;
    }

    public static FConstructor create(FVisibilityModifier modifier, FClass fClass, boolean predefined) {
        FConstructor res = new FConstructor(modifier, fClass, getParameters(fClass));
        if (predefined)
            res.predefined = true;
        else
            res.generateBody();
        return res;
    }

    @Override
    public boolean isConstructor() {
        return true;
    }

    //TODO @PositionForGeneratedCode
    private static ImmutableList<FParameter> getParameters(FClass fClass) {
        ImmutableList.Builder<FParameter> arguments = ImmutableList.builder();

        for (FField field : fClass.getInstanceFields().values()) {
            assert field.getAssignment().isEmpty();
            boolean canBeTreatedAsOptional = FOptional.canBeTreatedAsOptional(field.getType());
            FParameter parameter = FParameter.create(null, field.getIdentifier(), field.getType(), canBeTreatedAsOptional || field.hasAssignment());
            if (!field.hasAssignment() && canBeTreatedAsOptional)
                parameter.setDefaultValueTrusted(new FLiteralExpression(null, new FNull(parameter.getType())), emptySet());
            arguments.add(parameter);
        }

        return arguments.build();
    }

    //TODO @PositionForGeneratedCode
    private void generateBody() {
        DefaultNamespace memberOf = getMemberOf();
        FLocalVariable _this = new FLocalVariable(null, FIdentifier.THIS, memberOf.getType());

        FFunctionCall functionCall = FFunctionCall.createTrusted(null, Iterables.getOnlyElement(memberOf.getFunctions(false).get(MALLOC_ID)), Collections.emptyList());
        FAssignment thisDecl = FAssignment.createDecl(_this, functionCall);

        List<FExpression> fields = new ArrayList<>(getSignature().getParameters().size());
        List<FExpression> params = new ArrayList<>(getSignature().getParameters().size());
        for (FParameter param : getSignature().getParameters()) {
            FExpression thisExpr = new FVariableExpression(null, _this);
            FField field = memberOf.getType().getInstanceFields().get(param.getIdentifier());
            fields.add(FFunctionCall.createTrusted(null, field.getSetter().getLhsSignature(), mutableSingletonList(thisExpr)));
            params.add(new FVariableExpression(null, param));
        }
        FAssignment fieldAssign = FAssignment.createTrusted(null, fields, params);

        FReturn _return = FReturn.createTrusted(null, new FVariableExpression(null, _this), this);

        setBody(FBlock.from(null, thisDecl, fieldAssign, _return));
    }
}
