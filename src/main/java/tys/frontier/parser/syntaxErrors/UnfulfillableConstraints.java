package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;

public class UnfulfillableConstraints extends SyntaxError {

    public final FTypeVariable variable;
    public final TypeConstraints constraints;
    public final TypeConstraint a;
    public final TypeConstraint b;

    private UnfulfillableConstraints(String message, FTypeVariable variable, TypeConstraints constraints, TypeConstraint a, TypeConstraint b) {
        super(message);
        this.variable = variable;
        this.constraints = constraints;
        this.a = a;
        this.b = b;
    }

    public UnfulfillableConstraints(FTypeVariable variable, TypeConstraints constraints, TypeConstraint a, TypeConstraint b) {
        this("Constraints for " + variable + " are unfullfillable" +
                (a == null ? "" : ", contradicing: " + a + " and " + b),
                variable, constraints, a, b);
    }

    public static UnfulfillableConstraints empty(FTypeVariable variable, TypeConstraints constraints) {
        assert constraints.isEmpty();
        return new UnfulfillableConstraints("Cannot resolve " + variable + ", no constraints were given", variable, constraints, null, null);
    }
}
