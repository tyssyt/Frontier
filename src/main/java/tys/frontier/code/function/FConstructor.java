package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.code.type.FClass;

import java.util.*;

public class FConstructor extends FBaseFunction {

    public static final FFunctionIdentifier IDENTIFIER = new FFunctionIdentifier("!new");
    public static final FFunctionIdentifier MALLOC_ID = new FFunctionIdentifier("!malloc");

    private FConstructor(FVisibilityModifier modifier, FClass fClass, ImmutableList<FParameter> params) {
        super(IDENTIFIER, fClass, modifier, false, fClass, params);
    }

    public static FFunction createMalloc(FClass fClass) {
        FBaseFunction function = new FBaseFunction(MALLOC_ID, fClass, FVisibilityModifier.PRIVATE, false, fClass, ImmutableList.of());
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
        List<FParameter> arguments = new ArrayList<>();
        List<FParameter> defaultArguments = new ArrayList<>();

        for (FField field : fClass.getInstanceFields().values()) {
            if (field.getAssignment().isPresent()) {
                FExpression defaultValue = field.getAssignment().get();
                defaultArguments.add(FParameter.createTrusted(field.getIdentifier(), field.getType(), defaultValue));
            } else if (field.hasAssignment()) {
                defaultArguments.add(FParameter.create(field.getIdentifier(), field.getType(), true));
            } else if (FOptional.canBeTreatedAsOptional(field.getType())) {
                FExpression defaultValue = new FLiteralExpression(new FNull(field.getType()));
                defaultArguments.add(FParameter.createTrusted(field.getIdentifier(), field.getType(), defaultValue));
            } else {
                arguments.add(FParameter.create(field.getIdentifier(), field.getType(), false));
            }
        }

        arguments.sort(Comparator.comparing(o -> o.getIdentifier().name)); //TODO alphabetical order is far from a good choice here, but for now...
        defaultArguments.sort(Comparator.comparing(o -> o.getIdentifier().name));
        return new ImmutableList.Builder<FParameter>().addAll(arguments).addAll(defaultArguments).build();
    }

    private void generateBody() {
        FClass memberOf = (FClass) getMemberOf();
        FLocalVariable _this = new FLocalVariable(FVariableIdentifier.THIS, memberOf);

        FFunctionCall functionCall = FFunctionCall.createTrusted(Iterables.getOnlyElement(memberOf.getFunctions().get(MALLOC_ID)), Collections.emptyList());
        FVarAssignment thisDecl = FVarAssignment.createDecl(_this, functionCall);

        List<FVariableExpression> fields = new ArrayList<>(getParams().size());
        List<FExpression> params = new ArrayList<>(getParams().size());
        for (FParameter param : getParams()) {
            FExpression thisExpr = new FLocalVariableExpression(_this);
            FField field = memberOf.getInstanceFields().get(param.getIdentifier());
            fields.add(FFieldAccess.createInstanceTrusted(field, thisExpr));
            params.add(new FLocalVariableExpression(param));
        }
        FVarAssignment fieldAssign = FVarAssignment.createTrusted(fields, FVarAssignment.Operator.ASSIGN, params);

        FReturn _return = FReturn.createTrusted(Arrays.asList(new FLocalVariableExpression(_this)), this);

        setBody(FBlock.from(thisDecl, fieldAssign, _return));
    }
}
