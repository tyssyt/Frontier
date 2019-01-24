package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.typeInference.HasCall;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.util.NameGenerator;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class FTypeVariable implements FType {

    private FTypeIdentifier identifier;
    private Set<TypeConstraint> constraints;
    private Variance variance;
    private NameGenerator returnTypeNames = new NameGenerator("?" + identifier.name + "ret.", "");

    public FTypeVariable(FTypeIdentifier identifier) {
        this(identifier, Variance.Invariant);
    }
    public FTypeVariable(FTypeIdentifier identifier, Variance variance) {
        this(identifier, variance, new HashSet<>());
    }

    protected FTypeVariable(FTypeIdentifier identifier, Variance variance, Set<TypeConstraint> constraints) {
        this.identifier = identifier;
        this.variance = variance;
        this.constraints = constraints;
    }

    @Override
    public FTypeIdentifier getIdentifier() {
        return identifier;
    }

    public Variance getVariance() {
        return variance;
    }

    public boolean addConstraint(TypeConstraint constraint) {
        return constraints.add(constraint);
    }

    @Override
    public FFunction resolveFunction(FFunctionIdentifier identifier, List<FExpression> arguments, TypeInstantiation typeInstantiation) throws FunctionNotFound {
        HasCall constraint = new HasCall(null, identifier, arguments, typeInstantiation);
        constraints.add(constraint);

        //just return some fitting dummy function
        NameGenerator paramNames = new NameGenerator("?", "");
        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        for (FExpression arg : arguments) {
            FIdentifier id;
            if (arg.getType() == FTypeType.INSTANCE) {
                id = new FTypeIdentifier(paramNames.next());
            } else {
                id = new FVariableIdentifier(paramNames.next());
            }
            params.add(FParameter.create(id, arg.getType(), false));
        }
        FTypeVariable returnType = new FTypeVariable(new FTypeIdentifier(returnTypeNames.next()));
        //TODO what should the visibility be? I'm not sure if we check visibility when baking, so this might cause problems
        return new FFunction(identifier, this, FVisibilityModifier.EXPORT, true, returnType, params.build()) {
            @Override
            public boolean addCall(FFunctionCall call) { //this is a "hack" to set the origin of the constraint
                constraint.setOrigin(call);
                return super.addCall(call);
            }
        };
    }

    public FTypeVariable copy() {
        return new FTypeVariable(identifier, variance, new HashSet<>(constraints));
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }

    @Override
    public String toString() {
        return tS();
    }
}
