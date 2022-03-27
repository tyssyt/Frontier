package tys.frontier.code.expression;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Constraints;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.InvalidExpressionBinding;

import java.util.*;

public class UninstantiatedGenericFunctionAddress extends UnboundExpression {

    private FFunction uninstantiatedFunction;

    public UninstantiatedGenericFunctionAddress(Position position, FFunction uninstantiatedFunction) {
        super(position);
        this.uninstantiatedFunction = uninstantiatedFunction;
    }

    @Override
    public FFunctionType getType() {
        return FFunctionType.from(uninstantiatedFunction.getSignature());
    }

    @Override
    public Collection<FTypeVariable> getIn() {
        return uninstantiatedFunction.getParameters().values();
    }

    @Override
    public Optional<FTypeVariable> getOut() {
        // TODO actually check for out
        return Optional.empty();
    }

    @Override
    public FExpression bind(TypeInstantiation typeInstantiation) {
        assert typeInstantiation.fits(uninstantiatedFunction);
        return new FFunctionAddress(getPosition(), uninstantiatedFunction.getInstantiation(typeInstantiation));
    }

    @Override
    public FExpression bind(FType targetType) throws InvalidExpressionBinding {
        if (getType() == targetType)
            throw new InvalidExpressionBinding(this, targetType);

        Constraints constraints = new Constraints();
        try {
            ImplicitTypeCast.create(getType(), targetType, Variance.Invariant, constraints);
        } catch (IncompatibleTypes e) {
            throw new InvalidExpressionBinding(this, targetType);
        }

        Map<FTypeVariable, FType> typeMap = new HashMap<>();
        for (FTypeVariable param : uninstantiatedFunction.getParameters().values()) {
            List<ImplicitCastable> castables = constraints.get(param);
            assert castables.size() > 0 && castables.stream().allMatch(c -> c.getTarget() == castables.get(0).getTarget());
            typeMap.put(param, castables.get(0).getTarget());
        }

        return bind(TypeInstantiation.create(typeMap));
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(uninstantiatedFunction.getMemberOf().getIdentifier()).append('.').append(uninstantiatedFunction.getIdentifier()).append("**");
    }
}
