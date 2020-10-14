package tys.frontier.code.typeInference;

import com.google.common.collect.Multimap;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

import java.util.Map;

public abstract class TypeConstraint {

    private Object origin; //TODO an interface that groups all possible origins

    public TypeConstraint(Object origin) {
        this.origin = origin;
    }

    public Object getOrigin() {
        return origin;
    }

    public void setOrigin(Object origin) {
        this.origin = origin;
    }

    /* TODO
        When I copy TypeConstraints, I need to create copies of all constraints that are Type Variables.
        However this copied set of constraints needs to be consistent in itself, i.e. so copying non tree shaped constraint sets is difficult
        This error originally appeared for IsIterable constraints, where non tree shapes are impossible.
        Correct handling of this atm is near impossible, the next iteration of the constraint system should be build with this in mind.
     */
    public abstract TypeConstraint copy();

    @Override
    abstract public int hashCode();
    @Override
    abstract public boolean equals(Object obj);

    public static void addAll(Multimap<FTypeVariable, TypeConstraint> constraints) throws UnfulfillableConstraints {
        for (Map.Entry<FTypeVariable, TypeConstraint> entry : constraints.entries()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new UnfulfillableConstraints(entry.getKey().getConstraints(), entry.getValue(), null); //TODO
        }
    }

}
