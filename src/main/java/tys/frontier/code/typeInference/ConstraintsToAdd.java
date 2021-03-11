package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class ConstraintsToAdd {
    public ListMultimap<FTypeVariable, ImplicitCastable> implicitCasts;
    public Map<FTypeVariable, HasCall> hasCalls = new HashMap<>();

    public boolean isEmpty() {
        return implicitCasts.isEmpty() && hasCalls.isEmpty();
    }

    public boolean haveSameVars(ConstraintsToAdd other) {
        //implicitly assumes that the same key does not appear in both implicitCasts and hasCalls
        if (implicitCasts.keySet().size() + hasCalls.size() != other.implicitCasts.keySet().size() + other.hasCalls.size())
            return false;
        for (FTypeVariable typeVariable : implicitCasts.keySet())
            if (!other.implicitCasts.containsKey(typeVariable) && !other.hasCalls.containsKey(typeVariable))
                return false;
        for (FTypeVariable typeVariable : hasCalls.keySet())
            if (!other.implicitCasts.containsKey(typeVariable) && !other.hasCalls.containsKey(typeVariable))
                return false;
        return true;
    }

    public TypeConstraint removeSatisfiableCheckUnsatisfiable() {
        Iterator<Map.Entry<FTypeVariable, HasCall>> it = hasCalls.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<FTypeVariable, HasCall> entry = it.next();
            if (entry.getKey().getConstraints().satisfies(entry.getValue())) {
                it.remove(); //remove all satisfied constraints
                continue;
            }
            if (entry.getKey().isFixed()) {
                return entry.getValue(); //constraint is unsatisfiable
            }
            //otherwise the constraint stays in the set
        }
        return ImplicitCastable.removeSatisfiableCheckUnsatisfiable(implicitCasts);
    }

    public void addAll() throws UnfulfillableConstraints {
        ImplicitCastable.addAll(implicitCasts);
        for (Map.Entry<FTypeVariable, HasCall> entry : hasCalls.entrySet()) {
            if (!entry.getKey().tryAddConstraint(entry.getValue()))
                throw new UnfulfillableConstraints(entry.getKey().getConstraints(), entry.getValue(), null); //TODO
        }
    }

}
