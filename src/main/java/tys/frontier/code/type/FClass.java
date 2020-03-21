package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public abstract class FClass implements FType, HasVisibility {
    public abstract void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance);

    public abstract boolean isNative();

    public abstract FVisibilityModifier getConstructorVisibility();

    public abstract void setConstructorVisibility(FVisibilityModifier constructorVisibility);

    public abstract BiMap<FIdentifier, FField> getInstanceFields();

    public abstract BiMap<FIdentifier, FField> getStaticFields();

    public abstract List<? extends FType> getParametersList();

    public abstract Variance getParameterVariance(FTypeVariable parameter);

    public abstract Variance getParameterVariance(int i);

    public abstract FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments;

    public abstract Map<FType, FField> getDirectDelegates();

    public abstract void setForImpl(ForImpl forImpl);

    public abstract boolean isPredefined();

    @Override
    public abstract DefaultNamespace getNamespace();

    @Override
    public boolean canImplicitlyCast() {
        if (!getDirectDelegates().isEmpty())
            return true;
        for (int i = 0; i < getParametersList().size(); i++) {
            Variance var = getParameterVariance(i);
            if (var == Variance.Covariant && getParametersList().get(i).canImplicitlyCast())
                return true;
            if (var == Variance.Contravariant)
                return true;
        }
        return false;
    }

    public void addDelegate(FField field) throws DelegateFromTypeVar {
        assert field.getMemberOf() == this;
        if (!(field.getType() instanceof FClass))
            throw new DelegateFromTypeVar(field);
        if (field.getVisibility() != FVisibilityModifier.PRIVATE)
            getDirectDelegates().put(field.getType(), field);
    }

    public Pair<FField, ImplicitTypeCast> getDelegate(FType toType, Multimap<FTypeVariable, TypeConstraint> constraints) {
        FField res = getDirectDelegates().get(toType);
        if (res != null)
            return new Pair<>(res, null);
        for (Map.Entry<FType, FField> entry : getDirectDelegates().entrySet()) {
            try {
                ImplicitTypeCast outer = ImplicitTypeCast.create(entry.getKey(), toType, Variance.Covariant, constraints);
                return new Pair<>(entry.getValue(), outer);
            } catch (IncompatibleTypes ignored) {}
        }
        return null;
    }

    public Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    public void addField(FField field) throws IdentifierCollision, SignatureCollision {
        if (field.isInstance()) {
            FField old = getInstanceFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        } else {
            FField old = getStaticFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        }
        getNamespace().addFunction(field.getGetter());
        getNamespace().addFunction(field.getSetter());
    }

    public void addFieldTrusted(FField field) {
        try {
            addField(field);
        } catch (IdentifierCollision | SignatureCollision collision) {
            Utils.cantHappen();
        }
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getNamespace().getFunctions(false).get(FConstructor.IDENTIFIER)).getFunction();
    }

    public FConstructor generateConstructor() {
        FVisibilityModifier visibility = getConstructorVisibility() == null ? FVisibilityModifier.PRIVATE : getConstructorVisibility();
        try {
            getNamespace().addFunction(FConstructor.createMalloc(this));
            FConstructor res = FConstructor.create(visibility, this);
            getNamespace().addFunction(res);
            return res;
        } catch (SignatureCollision signatureCollision) {
            return Utils.handleException(signatureCollision);
        }
    }

    public void removeUnreachable(Reachability.ReachableNamespace reachable) {
        if (!isNative()) { //don't touch the structure of native classes, they have their layout for a reason
            getStaticFields().values().removeIf(f -> !reachable.isReachable(f));
            getInstanceFields().values().removeIf(f -> !reachable.isReachable(f));
        }
    }

    public <N, C,Fi,Fu,S,E> C accept(ClassVisitor<N, C, Fi, Fu, S, E> visitor) {
        visitor.enterClass(this);
        List<Fi> fields = new ArrayList<>(this.getInstanceFields().size() + this.getStaticFields().size());
        for (FField f : this.getInstanceFields().values()) {
            fields.add(f.accept(visitor));
        }
        for (FField f : this.getStaticFields().values()) {
            fields.add(f.accept(visitor));
        }
        return visitor.exitClass(this, fields);
    }

    @Override
    public String toString() {
        return getIdentifier().name;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }
}
