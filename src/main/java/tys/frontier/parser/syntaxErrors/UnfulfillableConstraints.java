package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.TypeConstraints;

public class UnfulfillableConstraints extends SyntaxError {

    public final FTypeVariable variable;
    public final TypeConstraints constraints;
    public final TypeConstraint a;
    public final TypeConstraint b;

    public UnfulfillableConstraints(FTypeVariable variable, TypeConstraints constraints, TypeConstraint a, TypeConstraint b) {
        super("Constraints for " + variable + " are unfullfillable" +
                (a == null ? "" : ", contradicing: " + a + " and " + b));
        this.variable = variable;
        this.constraints = constraints;
        this.a = a;
        this.b = b;
    }
}
