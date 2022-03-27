package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.typeInference.TypeConstraint;

public class UnfulfillableConstraints extends SyntaxError {

    public final Iterable<? extends TypeConstraint> constraints;

    public UnfulfillableConstraints(Iterable<? extends TypeConstraint> constraints) {
        super("Constraints are unfulfillable " + constraints);
        this.constraints = constraints;
    }
}
