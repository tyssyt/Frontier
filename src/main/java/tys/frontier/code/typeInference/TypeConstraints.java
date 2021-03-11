package tys.frontier.code.typeInference;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.statement.loop.forImpl.TupleFor;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.*;

import static tys.frontier.code.typeInference.Variance.*;

public class TypeConstraints {

    private FClass resolvedAs;
    private boolean fixed = false;

    private List<FTypeVariable> equivalenceGroup = new ArrayList<>();
    private List<TypeConstraints> implicitCastableCovariant = new ArrayList<>();
    private List<TypeConstraints> implicitCastableContravariant = new ArrayList<>();
    private List<FClass> implictCastableClassesCovariant = new ArrayList<>();
    private List<FClass> implictCastableClassesContravariant = new ArrayList<>();
    private List<HasCall> hasCalls = new ArrayList<>();
    private IsIterable isIterable;
    //private TypeConstraints isIterableParent;


    public TypeConstraints(FTypeVariable variable) {
        equivalenceGroup.add(variable);
    }

    public void copyContents(Map<TypeConstraints, TypeConstraints> constraintsMap, Queue<TypeConstraints> newConstraints) {
        assert !isResolved();
        TypeConstraints target = constraintsMap.get(this);

        for (TypeConstraints c : implicitCastableCovariant)
            target.implicitCastableCovariant.add(c.getOrCreateCopy(constraintsMap, newConstraints));
        for (TypeConstraints c : implicitCastableContravariant)
            target.implicitCastableContravariant.add(c.getOrCreateCopy(constraintsMap, newConstraints));
        target.implictCastableClassesCovariant.addAll(implictCastableClassesCovariant);
        target.implictCastableClassesContravariant.addAll(implictCastableClassesContravariant);
        target.hasCalls.addAll(hasCalls); //TODO I think if any of the argument Types are or contain type variables, I need to do more here (similar in the satisfyable check)
        if (isIterable != null) {
            if (isIterable.getElementType() instanceof FClass) {
                target.isIterable = isIterable;
            } else if (isIterable.getElementType() instanceof FTypeVariable) {
                TypeConstraints c = ((FTypeVariable) isIterable.getElementType()).getConstraints();
                TypeConstraints copy = c.getOrCreateCopy(constraintsMap, newConstraints);
                target.isIterable = new IsIterable(isIterable.getOrigin(), copy.equivalenceGroup.get(0));
            } else {
                Utils.cantHappen();
            }
        }
    }

    private TypeConstraints getOrCreateCopy(Map<TypeConstraints, TypeConstraints> constraintsMap, Queue<TypeConstraints> newConstraints) {
        TypeConstraints copy = constraintsMap.get(this);
        if (copy == null) {
            copy = equivalenceGroup.get(0).shallowNonFixedCopy().getConstraints();
            newConstraints.add(this);
            constraintsMap.put(this, copy);
        }
        return copy;
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

    public List<FTypeVariable> getEquivalenceGroup() {
        return equivalenceGroup;
    }

    public ForImpl getForImpl() {
        if (isResolved())
            return getResolved().getForImpl();
        if (isIterable == null && !isFixed()) {
            FTypeVariable elementType = new TypeVariableNamespace.IterationElementType(equivalenceGroup.get(0), false);
            isIterable = new IsIterable(null, elementType);
        }
        return isIterable;
    }

    @SuppressWarnings({"AssertWithSideEffects", "ConstantConditions"})
    private TypeConstraints merge(TypeConstraints other) {
        if ((other.fixed && !this.fixed) || (other.isResolved() && !this.isResolved())) {
            return other.merge(this);
        }

        //merge
        try {
            TypeConstraints _new = this;
            for (TypeConstraints c : other.implicitCastableCovariant)
                _new = add(_new, c, Covariant);
            for (TypeConstraints c : other.implicitCastableContravariant)
                _new = add(_new, c, Contravariant);
            for (FClass c : other.implictCastableClassesCovariant)
                _new.add(c, Covariant);
            for (FClass c : other.implictCastableClassesContravariant)
                _new.add(c, Contravariant);
            for (HasCall c : other.hasCalls)
                _new.add(c);
            if (other.isIterable != null)
                _new.add(other.isIterable);
            assert _new == this;
        } catch (UnfulfillableConstraints unfulfillableConstraints) {
            Utils.cantHappen();
        }
        this.fixed |= other.fixed;
        assert this.resolvedAs == other.resolvedAs; //there might be cases where this doesn't hold, wait for examples

        //update variables
        this.equivalenceGroup.addAll(other.equivalenceGroup);
        for (FTypeVariable otherVar : other.equivalenceGroup) {
            otherVar.setConstraints(this);
        }

        //force error in case there are somehow reference to other left (and have some fun with Java & asserts ;)
        assert (other.equivalenceGroup = null) == null;
        assert (other.implicitCastableCovariant = null) == null;
        assert (other.implicitCastableContravariant = null) == null;
        assert (other.implictCastableClassesCovariant = null) == null;
        assert (other.implictCastableClassesContravariant = null) == null;
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

    public TypeConstraints add(ImplicitCastable constraint) throws UnfulfillableConstraints {
        if (satisfies(constraint))
            return this;
        assert !fixed && !isResolved();

        if (constraint.getTarget() instanceof FTypeVariable) {
            return add(this, ((FTypeVariable) constraint.getTarget()).getConstraints(), constraint.getVariance());
        } else if (constraint.getTarget() instanceof FClass) {
            add((FClass) constraint.getTarget(), constraint.getVariance());
            return this;
        } else {
            return Utils.cantHappen();
        }
    }

    private void add(FClass target, Variance variance) throws UnfulfillableConstraints {
        //see if we can find an opposite constraint
        if (variance != Invariant && iterate(variance.opposite()).a.contains(target))
            variance = Invariant;

        //invariant means we can resolve
        if (variance == Invariant) {
            doResolve(target, false);
            return;
        }

        //finally add the constraint
        switch (variance) {
            case Covariant -> implictCastableClassesCovariant.add(target);
            case Contravariant -> implictCastableClassesContravariant.add(target);
        }
    }

    private static TypeConstraints add(TypeConstraints _this, TypeConstraints target, Variance variance) throws UnfulfillableConstraints {
        if (target.isResolved()) {
            _this.add(target.resolvedAs, variance);
            return _this;
        }

        //see if adding the constraint will create a cycle, if so merge all constraints on that cycle
        _this = findAndMergeCycles(_this, target, variance);

        //see if we can find an opposite constraint
        if (variance != Invariant && _this.iterate(variance.opposite()).b.contains(target))
            variance = Invariant;

        //invariant merge the two constraint groups or resolve one
        if (variance == Invariant)
            return _this.merge(target);

        //finally add the constraint
        switch (variance) {
            case Covariant -> _this.implicitCastableCovariant.add(target);
            case Contravariant -> _this.implicitCastableContravariant.add(target);
        }
        return _this;
    }


    public void add(HasCall constraint) throws UnfulfillableConstraints {
        if (satisfies(constraint))
            return;
        assert !fixed && !isResolved();
        hasCalls.add(constraint);
    }


    public void add(IsIterable constraint) throws UnfulfillableConstraints {
        if (satisfies(constraint))
            return;
        assert !fixed && !isResolved();

        if (isIterable != null) { //merge if a Iterable constraint already exists
            if (constraint.getElementType() instanceof FTypeVariable && isIterable.getElementType() instanceof FTypeVariable) {
                FTypeVariable oldElementType = (FTypeVariable) isIterable.getElementType();
                FTypeVariable newElementType = (FTypeVariable) constraint.getElementType();
                oldElementType.getConstraints().merge(newElementType.getConstraints());
            } else
                Utils.NYI("merge of IsIterable constraints, where at least one constraint has no TypeVariable as elementType"); //TODO can this even happen?
        } else
            isIterable = constraint;
    }

    public static TypeConstraints addAll(TypeConstraints _this, Iterable<ImplicitCastable> newConstraints) throws UnfulfillableConstraints {
        for (ImplicitCastable constraint : newConstraints) {
            _this = _this.add(constraint);
        }
        return _this;
    }

    private static TypeConstraints findAndMergeCycles(TypeConstraints _this, TypeConstraints newConstraints, Variance direction) {
        //TODO pretty sure I can at most find one cycle? think about it
        while (true) {
            if (direction != Contravariant) {
                ArrayList<TypeConstraints> path = new ArrayList<>();
                if (newConstraints.findCycle(_this, Covariant, path)) {
                    _this = mergeAll(path);
                    continue;
                }
            }
            if (direction != Covariant) {
                ArrayList<TypeConstraints> path = new ArrayList<>();
                if (newConstraints.findCycle(_this, Contravariant, path)) {
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
        List<TypeConstraints> implictCastable = direction == Covariant ? implicitCastableCovariant : implicitCastableContravariant;
        if (implictCastable.contains(goal)) {
            path.add(goal);
            return true;
        }
        for (TypeConstraints typeConstraints : implictCastable) {
            if (typeConstraints.findCycle(goal, direction, path))
                return true;
        }
        path.remove(path.size() - 1);
        return false;
    }

    public FType softResolve(Multimap<FTypeVariable, ImplicitCastable> newConstraints) throws UnfulfillableConstraints {
        FClass resolve = resolve(true, newConstraints);
        if (resolve == null)
            return equivalenceGroup.iterator().next();
        return resolve;
    }

    private static final ListMultimap<FTypeVariable, ImplicitCastable> EMPTY = MultimapBuilder.hashKeys().arrayListValues().build();
    public FClass hardResolve() throws UnfulfillableConstraints {
        FClass res = resolve(false, EMPTY);
        assert res != null && EMPTY.isEmpty();
        return res;
    }

    private FClass resolve(boolean soft, Multimap<FTypeVariable, ImplicitCastable> newConstraints) throws UnfulfillableConstraints {
        if (isResolved())
            return resolvedAs;
        FClass proposition = this.proposeType(iterate(Contravariant).a, iterate(Covariant).a);
        if (proposition == null)
            return null;
        //resolve
        newConstraints.putAll(this.doResolve(proposition, soft));
        return proposition;
    }

    public FClass getResolved() {
        assert isResolved();
        return resolvedAs;
    }

    private ListMultimap<FTypeVariable, ImplicitCastable> doResolve(FClass proposition, boolean soft) throws UnfulfillableConstraints {
        //check against constraints, error if not consistent
        ListMultimap<FTypeVariable, ImplicitCastable> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (TypeConstraints constraints : implicitCastableCovariant)
            if (!implies(proposition, constraints.equivalenceGroup.get(0), Covariant, newConstraints))
                throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), new ImplicitCastable(null, constraints.equivalenceGroup.get(0), Covariant));
        for (TypeConstraints constraints : implicitCastableContravariant)
            if (!implies(proposition, constraints.equivalenceGroup.get(0), Contravariant, newConstraints))
                throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), new ImplicitCastable(null, constraints.equivalenceGroup.get(0), Contravariant));
        for (FClass fClass : implictCastableClassesCovariant)
            if (!implies(proposition, fClass, Covariant, newConstraints))
                throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), new ImplicitCastable(null, fClass, Covariant));
        for (FClass fClass : implictCastableClassesContravariant)
            if (!implies(proposition, fClass, Contravariant, newConstraints))
                throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), new ImplicitCastable(null, fClass, Contravariant));

        for (HasCall c : hasCalls)
            if (!implies(proposition, c))
                throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), c);

        if (isIterable != null && !implies(proposition, isIterable, newConstraints))
            throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), isIterable);

        //we might create constraint for the class we are resolving right now, handle those first
        ListMultimap<FTypeVariable, ImplicitCastable> evenNewerConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (FTypeVariable eqVar : equivalenceGroup) {
            for (ImplicitCastable c : newConstraints.removeAll(eqVar)) {
                if (!implies(proposition, c, evenNewerConstraints) || !evenNewerConstraints.isEmpty())
                    throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), c);
            }
        }

        //remove all satisfiable new constraints
        ImplicitCastable unsatisfiable = ImplicitCastable.removeSatisfiableCheckUnsatisfiable(newConstraints);
        if (unsatisfiable != null) {
            throw new UnfulfillableConstraints(this, new ImplicitCastable(this, proposition, Invariant), unsatisfiable);
        }

        if (!soft) {
            for (Map.Entry<FTypeVariable, Collection<ImplicitCastable>> entry : newConstraints.asMap().entrySet()) {
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

    private Pair<Set<FClass>, Set<TypeConstraints>> iterate(Variance direction) {
        HashSet<FClass> resClasses = new HashSet<>();
        HashSet<TypeConstraints> resVariables = new HashSet<>();
        iterate(direction, resClasses, resVariables);
        return new Pair<>(resClasses, resVariables);
    }

    private void iterate(Variance direction, Set<FClass> resClasses, Set<TypeConstraints> resVariables) {
        assert direction != Invariant;
        if (isResolved()) {
            resClasses.add(resolvedAs);
            return;
        }

        List<TypeConstraints> implictCastable = direction == Covariant ? implicitCastableCovariant : implicitCastableContravariant;
        List<FClass> implictCastableClasses = direction == Covariant ? implictCastableClassesCovariant : implictCastableClassesContravariant;

        resClasses.addAll(implictCastableClasses);
        for (TypeConstraints typeConstraints : implictCastable) {
            if (resVariables.add(typeConstraints))
                typeConstraints.iterate(direction, resClasses, resVariables);
        }
    }

    private FClass proposeType(Set<FClass> contra, Set<FClass> co) {
        //just pick the most concrete out of all upper and lower Bounds for now TODO improve: change mindset to find the most concrete type you can, but not more!
        FClass maxContra = contra.isEmpty() ? null : Collections.max(contra, Comparator.comparingInt(FClass::concreteness));
        FClass maxCo     =     co.isEmpty() ? null : Collections.max(co,     Comparator.comparingInt(FClass::concreteness));
        if (maxContra == null && maxCo == null)
            return null;
        if (maxContra == null)
            return maxCo;
        if (maxCo == null)
            return maxContra;
        return maxContra.concreteness() > maxCo.concreteness() ? maxContra : maxCo;
    }

    public boolean satisfies(ImplicitCastable implicitCastable) {
        if (isResolved()) {
            if (implicitCastable.getTarget() instanceof FTypeVariable) {
                FTypeVariable target = (FTypeVariable) implicitCastable.getTarget();
                if (!target.isFixed()) {
                    //for implicit castable to a type var, we can just add the reverse constraint to that var
                    return target.tryAddConstraint(new ImplicitCastable(implicitCastable, resolvedAs, implicitCastable.getVariance().opposite()));
                }
            }
            ListMultimap<FTypeVariable, ImplicitCastable> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
            return implies(resolvedAs, implicitCastable, newConstraints) && newConstraints.isEmpty();
        }

        if (implicitCastable.getVariance() == Invariant) {
            //TODO maybe do an if instanceof FTypeVariable?
            //noinspection SuspiciousMethodCalls
            return equivalenceGroup.contains(implicitCastable.getTarget());
        }

        //search
        Pair<Set<FClass>, Set<TypeConstraints>> iteratePair = iterate(implicitCastable.getVariance());
        iteratePair.b.add(this);

        {
            FType target = implicitCastable.getTarget();
            if (target instanceof FTypeVariable) { //special case if the target is a type variable
                for (TypeConstraints v : iteratePair.b) {
                    if (v.equivalenceGroup.contains(target))
                        return true;
                }
            }
            if (target instanceof FOptional && ((FOptional) target).getBaseType() instanceof FTypeVariable) { //even more special case
                FTypeVariable targetBase = (FTypeVariable) ((FOptional) target).getBaseType();
                for (TypeConstraints v : iteratePair.b) {
                    if (v.equivalenceGroup.contains(targetBase))
                        return true;
                }
            }
        }

        //default case: check with implies
        ListMultimap<FTypeVariable, ImplicitCastable> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (FClass c : iteratePair.a) {
            if (implies(c, implicitCastable, newConstraints) && newConstraints.isEmpty())
                return true;
            newConstraints.clear();
        }
        return false;
    }
    public boolean satisfies(HasCall hasCall) {
        if (isResolved())
            return implies(resolvedAs, hasCall);
        if (hasCalls.contains(hasCall))
            return true;
        for (FClass c : iterate(Covariant).a)
            if (implies(c, hasCall))
                return true;
        return false;
    }
    public boolean satisfies(IsIterable isIterable) {
        if (isResolved()) {
            ListMultimap<FTypeVariable, ImplicitCastable> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
            return implies(resolvedAs, isIterable, newConstraints) && newConstraints.isEmpty();
        }
        if (isIterable.equals(this.isIterable))
            return true;
        ListMultimap<FTypeVariable, ImplicitCastable> newConstraints = MultimapBuilder.hashKeys().arrayListValues().build();
        for (FClass c : iterate(Covariant).a) {
            if (implies(c, isIterable, newConstraints) && newConstraints.isEmpty())
                return true;
            newConstraints.clear();
        }
        return false;
    }

    private static boolean implies(FClass a, ImplicitCastable b, Multimap<FTypeVariable, ImplicitCastable> newConstraints) {
        return implies(a, b.getTarget(), b.getVariance(), newConstraints);
    }
    private static boolean implies(FClass a, FType b, Variance variance, Multimap<FTypeVariable, ImplicitCastable> newConstraints) {
        if (b instanceof FTypeVariable && ((FTypeVariable) b).isResolved())
            b = ((FTypeVariable) b).getResolved();
        try {
            if (a == b)
                return true;
            ImplicitTypeCast.create(a, b, variance, newConstraints);
            return true;
        } catch (IncompatibleTypes incompatibleTypes) {
            return false;
        }
    }
    private static boolean implies(FClass a, HasCall b) {
        try {
            if (b instanceof HasSelfCall) {
                ((HasSelfCall) b).resolve(a.getNamespace()); //TODO this is slightly dumb for similar reasons as below
            } else if (b instanceof HasRemoteCall) {
                ((HasRemoteCall) b).resolve(); //TODO this is really dumb and not really what we need here...
            } else {
                return Utils.cantHappen();
            }
            return true;
        } catch (FunctionNotFound functionNotFound) {
            return false;
        }
    }
    private static boolean implies(FClass a, IsIterable b, Multimap<FTypeVariable, ImplicitCastable> newConstraints) {
        ForImpl forImpl = a.getForImpl();
        if (forImpl == null)
            return false;
        if (forImpl instanceof TupleFor) //TODO I don't like this special logic, but Tuple for is a big hack...
            try {
                ImplicitTypeCast.create(forImpl.getElementType(), b.getElementType(), Covariant, newConstraints);
                return true;
            } catch (IncompatibleTypes incompatibleTypes) {
                return false;
            }

        assert forImpl.getElementType() instanceof FClass;
        return implies((FClass) forImpl.getElementType(), b.getElementType(), Covariant, newConstraints);
    }
}
