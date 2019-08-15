package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;

public class UnfulfillableConstraints extends SyntaxError {

    public final TypeConstraints constraints;
    public final TypeConstraint a;
    public final TypeConstraint b;

    private UnfulfillableConstraints(String message, TypeConstraints constraints, TypeConstraint a, TypeConstraint b) {
        super(message);
        this.constraints = constraints;
        this.a = a;
        this.b = b;
    }

    public UnfulfillableConstraints(TypeConstraints constraints, TypeConstraint a, TypeConstraint b) {
        this("Constraints are unfulfillable" +
                (a == null ? "" : ", contradicing: " + a + " and " + b),
                constraints, a, b);
    }
}
