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
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FClass;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;

public class FConstructor extends FBaseFunction {

    public static final FIdentifier IDENTIFIER = new FIdentifier("!new");
    public static final FIdentifier MALLOC_ID = new FIdentifier("!malloc");

    private FConstructor(FVisibilityModifier modifier, FClass fClass, ImmutableList<FParameter> params) {
        super(IDENTIFIER, fClass, modifier, false, fClass, params, null, emptyMap());
    }

    public static FFunction createMalloc(FClass fClass) {
        FBaseFunction function = new FBaseFunction(MALLOC_ID, fClass, FVisibilityModifier.PRIVATE, false, fClass, ImmutableList.of(), null, emptyMap());
        function.predefined = true;
        return function;
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
        ImmutableList.Builder<FParameter> arguments = ImmutableList.builder();

        for (FField field : fClass.getInstanceFields().values()) {
            assert !field.getAssignment().isPresent();
            boolean canBeTreatedAsOptional = FOptional.canBeTreatedAsOptional(field.getType());
            FParameter parameter = FParameter.create(field.getIdentifier(), field.getType(), canBeTreatedAsOptional || field.hasAssignment());
            if (!field.hasAssignment() && canBeTreatedAsOptional)
                parameter.setDefaultValueTrusted(new FLiteralExpression(new FNull(parameter.getType())), emptySet());
            arguments.add(parameter);
        }

        return arguments.build();
    }

    private void generateBody() {
        FClass memberOf = (FClass) getMemberOf();
        FLocalVariable _this = new FLocalVariable(FIdentifier.THIS, memberOf);

        FFunctionCall functionCall = FFunctionCall.createTrusted(Iterables.getOnlyElement(memberOf.getFunctions(false).get(MALLOC_ID)), Collections.emptyList());
        FAssignment thisDecl = FAssignment.createDecl(_this, functionCall);

        List<FExpression> fields = new ArrayList<>(getSignature().getParameters().size());
        List<FExpression> params = new ArrayList<>(getSignature().getParameters().size());
        for (FParameter param : getSignature().getParameters()) {
            FExpression thisExpr = new FLocalVariableExpression(_this);
            FField field = memberOf.getInstanceFields().get(param.getIdentifier());
            fields.add(FFunctionCall.createTrusted(field.getSetter().getLhsSignature(), asList(thisExpr)));
            params.add(new FLocalVariableExpression(param));
        }
        FAssignment fieldAssign = FAssignment.createTrusted(fields, params);

        FReturn _return = FReturn.createTrusted(new FLocalVariableExpression(_this), this);

        setBody(FBlock.from(thisDecl, fieldAssign, _return));
    }
}
