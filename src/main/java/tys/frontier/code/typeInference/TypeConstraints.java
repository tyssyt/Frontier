package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;

import java.util.*;

import static tys.frontier.code.typeInference.Variance.*;

public class TypeConstraints {

    private List<FTypeVariable> equivalenceGroup = new ArrayList<>();
    private FClass resolvedAs;
    private boolean fixed = false;
    private List<TypeConstraint> constraints;

    private TypeConstraints(List<TypeConstraint> constraints) {
        this.constraints = constraints;
    }

    public static TypeConstraints create() {
        return new TypeConstraints(new ArrayList<>());
    }

    public TypeConstraints copy() {
        return new TypeConstraints(new ArrayList<>(constraints));
    }

    public void addVar(FTypeVariable var) {
        equivalenceGroup.add(var);
    }

    public boolean isEmpty() {
        return constraints.isEmpty();
    }

    public boolean isResolved() {
        return resolvedAs != null;
    }

    public boolean isFixed() {
        return fixed;
    }

    public void setFixed() {
        this.fixed = true;
    }

    @SuppressWarnings("AssertWithSideEffects")
    private TypeConstraints merge(TypeConstraints other) {
        if ((other.fixed && !this.fixed) || (other.isResolved() && !this.isResolved())) {
            return other.merge(this);
        }

        //merge
        try {
            TypeConstraints _new = addAll(this, other.constraints);
            assert _new == this;
        } catch (UnfulfillableConstraints unfulfillableConstraints) {
            Utils.cantHappen();
        }
        this.fixed |= other.fixed;
        assert this.resolvedAs == other.resolvedAs; //there might be cases where this doesn't hold, wait for examples

        //update variables
        for (FTypeVariable otherVar : other.equivalenceGroup) {
            otherVar.setConstraints(this);
        }
        this.equivalenceGroup.addAll(other.equivalenceGroup);

        //force error in case there are somehow reference to other left (and have some fun with Java & asserts ;)
        assert (other.equivalenceGroup = null) == null;
        assert (other.constraints = null) == null;
        return this;
    }

    private static TypeConstraints mergeAll(Collection<TypeConstraints> constraints) {
        Iterator<TypeConstraints> it = constraints.iterator();
        TypeConstraints res = it.next();
        while (it.hasNext()) {
            res = res.merge(it.next());
        }
        return res;
    }

    public static TypeConstraints add(TypeConstraints _this, TypeConstraint constraint) throws UnfulfillableConstraints {
        if (_this.satisfies(constraint))
            return _this;
        assert !_this.fixed && !_this.isResolved();

        if (constraint instanceof ImplicitCastable) {
            ImplicitCastable implicitCastable = (ImplicitCastable) constraint;

            //if the target is a resolved var, change the constraint to the resolved
            if (implicitCastable.getTarget() instanceof FTypeVariable) {
                FTypeVariable target = (FTypeVariable) implicitCastable.getTarget();
                if (target.getConstraints().isResolved()) {
                    implicitCastable = new ImplicitCastable(implicitCastable, target.getConstraints().resolvedAs, implicitCastable.getVariance());
                }
            }

            //see if adding the constraint will create a cycle, if so merge all constraints on that cycle
            if (implicitCastable.getTarget() instanceof FTypeVariable) {
                _this = findAndMergeCycles(_this, implicitCastable);
            }

            //see if we can find an opposite constraint
            if (implicitCastable.getVariance() != Invariant) {
                //iterate in the opposite direction of the constraint
                Set<FClass> oppClasses = new HashSet<>();
                Set<TypeConstraints> oppVars = new HashSet<>();
                _this.iterate(implicitCastable.getVariance().opposite(), oppClasses, oppVars);

                if (implicitCastable.getTarget() instanceof FClass) { //for class target check the visited classes
                    FClass target = (FClass) implicitCastable.getTarget();
                    if (oppClasses.contains(target)) {
                        //upgrade the constraint to invariant
                        implicitCastable = new ImplicitCastable(implicitCastable, implicitCastable.getTarget(), Invariant);
                    }
                } else if (implicitCastable.getTarget() instanceof FTypeVariable) { //for var targets check visited vars
                    FTypeVariable target = (FTypeVariable) implicitCastable.getTarget();
                    for (TypeConstraints oppVar : oppVars) {
                        if (oppVar.equivalenceGroup.contains(target)) {
                            //upgrade the constraint to invariant
                            implicitCastable = new ImplicitCastable(implicitCastable, implicitCastable.getTarget(), Invariant);
                            break;
                        }
                    }
                } else {
                    return Utils.cantHappen();
                }
            }

            //invariant means we can either merge two constraint groups or resolve one
            if (implicitCastable.getVariance() == Invariant) {
                if (implicitCastable.getTarget() instanceof FClass) {
                    //resolve
                    _this.doResolve((FClass) implicitCastable.getTarget(), false);
                    return _this;
                } else if (implicitCastable.getTarget() instanceof FTypeVariable) {
                    //merge
                    return _this.merge(((FTypeVariable) implicitCastable.getTarget()).getConstraints());
                } else {
                    return Utils.cantHappen();
                }
            }

            //remove constraints implied by the to be added
            ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
            for (Iterator<TypeConstraint> it = _this.constraints.iterator(); it.hasNext();) {
                TypeConstraint c = it.next();
                if (implies(implicitCastable, c, newConstraints) && newConstraints.isEmpty())
                    it.remove();
                newConstraints.clear();
            }

            //finally add the constraint
            _this.constraints.add(implicitCastable);
            return _this;
        } else if (constraint instanceof HasCall) {
            _this.constraints.add(constraint);
            return _this;
        } else {
            return Utils.cantHappen();
        }
    }

    public static TypeConstraints addAll(TypeConstraints _this, Collection<TypeConstraint> newConstraints) throws UnfulfillableConstraints {
        for (TypeConstraint constraint : newConstraints) {
            _this = add(_this, constraint);
        }
        return _this;
    }

    private static TypeConstraints findAndMergeCycles(TypeConstraints _this, ImplicitCastable constraint) {
        while (true) {
            TypeConstraints start = ((FTypeVariable) constraint.getTarget()).getConstraints();
            if (constraint.getVariance() != Contravariant) {
                ArrayList<TypeConstraints> path = new ArrayList<>();
                if (start.findCycle(_this, Covariant, path)) {
                    _this = mergeAll(path);
                    continue;
                }
            }
            if (constraint.getVariance() != Covariant) {
                ArrayList<TypeConstraints> path = new ArrayList<>();
                if (start.findCycle(_this, Contravariant, path)) {
                    _this = mergeAll(path);
                    continue;
                }
            }
            break;
        }
        return _this;
    }

    private boolean findCycle(TypeConstraints goal, Variance direction, ArrayList<TypeConstraints> path) {
        path.add(this);
        for (TypeConstraint constraint : constraints) {
            if (constraint instanceof ImplicitCastable) {
                ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
                if (implicitCastable.getVariance() == direction && implicitCastable.getTarget() instanceof FTypeVariable) {
                    TypeConstraints constraints = ((FTypeVariable) implicitCastable.getTarget()).getConstraints();
                    if (constraints == goal) {
                        path.add(goal);
                        return true;
                    } else {
                        if (((FTypeVariable) implicitCastable.getTarget()).getConstraints().findCycle(goal, direction, path))
                            return true;
                    }
                }
            }
        }
        path.remove(path.size() - 1);
        return false;
    }

    public boolean satisfies(TypeConstraint constraint) {
        if (isResolved()) {
            if (constraint instanceof ImplicitCastable && ((ImplicitCastable) constraint).getTarget() instanceof FTypeVariable) {
                //for implicit castable to a type var, we can just add the reverse constraint to that var
                ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
                return ((FTypeVariable) implicitCastable.getTarget()).tryAddConstraint(
                        new ImplicitCastable(constraint, equivalenceGroup.iterator().next(), implicitCastable.getVariance().opposite())
                );
            }
            ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
            return implies(new ImplicitCastable(this, resolvedAs, Invariant), constraint, newConstraints) && newConstraints.isEmpty();
        }

        Variance direction;
        if (constraint instanceof ImplicitCastable) {
            direction = ((ImplicitCastable) constraint).getVariance();
            if (direction == Invariant) {
                //noinspection SuspiciousMethodCalls
                return equivalenceGroup.contains(((ImplicitCastable) constraint).getTarget());
            }
        } else if (constraint instanceof HasCall) {
            direction = Covariant;
        } else {
            return Utils.cantHappen();
        }

        //search
        Set<FClass> classes = new HashSet<>();
        Set<TypeConstraints> vars = new HashSet<>();
        iterate(direction, classes, vars);
        vars.add(this);

        if (constraint instanceof ImplicitCastable) {
            FType target = ((ImplicitCastable) constraint).getTarget();
            if (target instanceof FTypeVariable) { //special case if the target is a type variable
                for (TypeConstraints v : vars) {
                    if (v.equivalenceGroup.contains(target))
                        return true;
                }
            }
            if (target instanceof FOptional && ((FOptional) target).getBaseType() instanceof FTypeVariable) { //even more special case
                FTypeVariable targetBase = (FTypeVariable) ((FOptional) target).getBaseType();
                for (TypeConstraints v : vars) {
                    if (v.equivalenceGroup.contains(targetBase))
                        return true;
                }
            }
        }

        //default case: check with implies
        ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (FClass c : classes) {
            if (implies(new ImplicitCastable(null, c, direction), constraint, newConstraints) && newConstraints.isEmpty())
                return true;
            newConstraints.clear();
        }
        return false;
    }

    public FType softResolve(Multimap<FTypeVariable, TypeConstraint> newConstraints) throws UnfulfillableConstraints {
        FClass resolve = resolve(true, newConstraints);
        if (resolve == null)
            return equivalenceGroup.iterator().next();
        return resolve;
    }

    private static final ListMultimap<FTypeVariable, TypeConstraint> EMPTY = MultimapBuilder.hashKeys().arrayListValues().build();
    public FClass hardResolve() throws UnfulfillableConstraints {
        FClass res = resolve(false, EMPTY);
        assert res != null && EMPTY.isEmpty();
        return res;
    }

    private FClass resolve(boolean soft, Multimap<FTypeVariable, TypeConstraint> newConstraints) throws UnfulfillableConstraints {
        if (isResolved())
            return resolvedAs;

        // collect all upper bounds (contravariant constraints) including transitive ones, same for lowers
        Set<FClass> contraClasses = new HashSet<>();
        Set<TypeConstraints> contraVars = new HashSet<>();
        iterate(Contravariant, contraClasses, contraVars);
        Set<FClass> coClasses = new HashSet<>();
        Set<TypeConstraints> coVars = new HashSet<>();
        iterate(Covariant, coClasses, coVars);

        //use bounds to find most concrete type proposition
        FClass proposition = this.proposeType(contraClasses, coClasses);

        if (proposition == null)
            return null;

        //resolve
        newConstraints.putAll(this.doResolve(proposition, soft));
        return proposition;
    }

    public FType getResolved() {
        assert isResolved();
        return resolvedAs;
    }

    private ListMultimap<FTypeVariable, TypeConstraint> doResolve(FClass proposition, boolean soft) throws UnfulfillableConstraints {
        //check against constraints, error if not consistent
        ListMultimap<FTypeVariable, TypeConstraint> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        ImplicitCastable typeConstraint = new ImplicitCastable(this, proposition, Invariant);
        for (TypeConstraint constraint : this.constraints) {
            if (!implies(typeConstraint, constraint, newConstraints)) {
                throw new UnfulfillableConstraints(this, typeConstraint, constraint);
            }
        }

        //we might create constraint for the class we are resolving right now, handle those first
        ListMultimap<FTypeVariable, TypeConstraint> evenNewerConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (FTypeVariable eqVar : equivalenceGroup) {
            for (TypeConstraint c : newConstraints.removeAll(eqVar)) {
                if (!implies(typeConstraint, c, evenNewerConstraints) || !evenNewerConstraints.isEmpty())
                    throw new UnfulfillableConstraints(this, typeConstraint, c);
            }
        }

        //remove all satisfiable new constraints
        TypeConstraint unsatisfiable = removeSatisfiableCheckUnsatisfiable(newConstraints);
        if (unsatisfiable != null) {
            throw new UnfulfillableConstraints(this, typeConstraint, unsatisfiable);
        }

        if (!soft) {
            for (Map.Entry<FTypeVariable, Collection<TypeConstraint>> entry : newConstraints.asMap().entrySet()) {
                TypeConstraints.addAll(entry.getKey().getConstraints(), entry.getValue());
            }
            newConstraints.clear();
        }

        if (newConstraints.isEmpty()) {
            //if there are no more new constraints, mark this group as resolved
            this.resolvedAs = proposition;
        }
        //TODO consider caching the result and constraints that need to be fulfilled
        return newConstraints;
    }

    private void iterate(Variance direction, Set<FClass> resClasses, Set<TypeConstraints> resVariables) {
        assert direction != Invariant;
        //because we merge cycles when adding constraints, there cannot be any cycles!
        for (TypeConstraint constraint : constraints) {
            if (constraint instanceof ImplicitCastable) {
                ImplicitCastable implicitCastable = (ImplicitCastable) constraint;
                if (implicitCastable.getVariance() == direction) {
                    if (implicitCastable.getTarget() instanceof FTypeVariable) {
                        TypeConstraints transitiveConstraints = ((FTypeVariable) implicitCastable.getTarget()).getConstraints();
                        if (transitiveConstraints.isResolved())
                            resClasses.add(transitiveConstraints.resolvedAs); //treat resolved constraints like a class
                        else if (resVariables.add(transitiveConstraints))
                            transitiveConstraints.iterate(direction, resClasses, resVariables); //recurse
                    } else if (implicitCastable.getTarget() instanceof FClass) {
                        resClasses.add((FClass) implicitCastable.getTarget()); //add res
                    } else {
                        Utils.cantHappen();
                    }
                }
            }
        }
    }

    private FClass proposeType(Set<FClass> contra, Set<FClass> co) {
        //just pick the most concrete out of all upper and lower Bounds for now TODO improve: change mindset to find the most concrete type you can, but not more!
        FClass maxContra = contra.isEmpty() ? null : Collections.max(contra, Comparator.comparingLong(FClass::concreteness));
        FClass maxCo     =     co.isEmpty() ? null : Collections.max(co,     Comparator.comparingLong(FClass::concreteness));
        if (maxContra == null && maxCo == null)
            return null;
        if (maxContra == null)
            return maxCo;
        if (maxCo == null)
            return maxContra;
        return maxContra.concreteness() > maxCo.concreteness() ? maxContra : maxCo;
    }


    public static boolean implies(ImplicitCastable a, TypeConstraint b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        if (b instanceof ImplicitCastable)
            return implies(a, (ImplicitCastable) b, newConstraints);
        else if (b instanceof HasCall)
            return implies(a, (HasCall) b, newConstraints);
        else
            return Utils.cantHappen();
    }

    public static boolean implies(ImplicitCastable a, ImplicitCastable b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        if (a.getVariance().sign * b.getVariance().sign == -1) //a covariant and b contravariant or vice versa
            return false;

        FType aTarget = a.getTarget();
        if (aTarget instanceof FTypeVariable && ((FTypeVariable) aTarget).getConstraints().isResolved())
            aTarget = ((FTypeVariable) aTarget).getConstraints().resolvedAs;
        FType bTarget = b.getTarget();
        if (bTarget instanceof FTypeVariable && ((FTypeVariable) bTarget).getConstraints().isResolved())
            bTarget = ((FTypeVariable) bTarget).getConstraints().resolvedAs;
        try {
            if (aTarget == bTarget)
                return true;
            ImplicitTypeCast.create(aTarget, bTarget, b.getVariance(), newConstraints);
            return true;
        } catch (IncompatibleTypes incompatibleTypes) {
            return false;
        }
    }

    public static boolean implies(ImplicitCastable a, HasCall b, Multimap<FTypeVariable, TypeConstraint> newConstraints) {
        if (a.getVariance() == Contravariant) //casts from constraints cannot be used for function resolving
            return false;

        FType aTarget = a.getTarget();
        if (aTarget instanceof FTypeVariable && ((FTypeVariable) aTarget).getConstraints().isResolved())
            aTarget = ((FTypeVariable) aTarget).getConstraints().resolvedAs;
        try {
            //TODO I have no Idea how/if the variance of a should be considered in resolving
            aTarget.softResolveFunction(b.getIdentifier(), b.getPositionalArgs(), b.getKeywordArgs(), null, b.isLhsResolve());
            return true;
        } catch (FunctionNotFound functionNotFound) {
            return false;
        }
    }

    public static TypeConstraint removeSatisfiableCheckUnsatisfiable(Multimap<FTypeVariable, TypeConstraint> constraints) {
        Iterator<Map.Entry<FTypeVariable, TypeConstraint>> it = constraints.entries().iterator();
        while (it.hasNext()) {
            Map.Entry<FTypeVariable, TypeConstraint> entry = it.next();
            if (entry.getKey().getConstraints().satisfies(entry.getValue())) {
                it.remove(); //remove all satisfied constraints
                continue;
            }
            if (entry.getKey().isFixed()) {
                return entry.getValue(); //constraint is unsatisfiable
            }
            //otherwise the constraint stays in the set
        }
        return null;
    }
}
