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
