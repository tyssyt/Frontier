package tys.frontier.code.typeInference;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.util.Utils;

import java.util.*;

public class TypeConstraints {

    Multimap<FTypeVariable, TypeConstraint> constraints = ArrayListMultimap.create();

    public TypeConstraints() {}

    public void put (FTypeVariable variable, TypeConstraint constraint) {
        constraints.put(variable, constraint);
    }

    public List<TypeInstantiation> solve() {
        Map<FTypeVariable, FType> typeMap = new HashMap<>();
        for (Map.Entry<FTypeVariable, Collection<TypeConstraint>> entry : constraints.asMap().entrySet()) {
            FTypeVariable var = entry.getKey();
            FType type = null;

            for (TypeConstraint constraint : entry.getValue()) {
                if (constraint instanceof IsType) {
                    FType target = ((IsType) constraint).getTarget();
                    if (type == target)
                        continue;
                    if (type == null)
                        type = target;
                    else
                        return Collections.emptyList(); //TODO report clashing constraints, maybe store them in this class and have a method that can extract them?
                } else if (constraint instanceof ImplicitCastable) {
                    return Utils.NYI("implict castable type constraint"); //TODO
                } else {
                    return Utils.cantHappen();
                }
            }
            assert type != null;
            typeMap.put(var, type);
        }
        return Collections.singletonList(TypeInstantiation.create(typeMap));
    }
}
