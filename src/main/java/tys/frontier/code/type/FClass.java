package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public interface FClass extends FType, HasVisibility {
    void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance);

    @Override
    long concreteness();

    @Override
    FTypeIdentifier getIdentifier();

    @Override
    FVisibilityModifier getVisibility();

    FVisibilityModifier getConstructorVisibility();

    void setConstructorVisibility(FVisibilityModifier constructorVisibility);

    BiMap<FIdentifier, FField> getInstanceFields();

    BiMap<FIdentifier, FField> getStaticFields();

    Multimap<FFunctionIdentifier, FFunction> getFunctions();

    List<? extends FType> getParametersList();

    Variance getParameterVariance(FTypeVariable parameter);

    Variance getParameterVariance(int i);

    FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments;

    Map<FType, FField> getDirectDelegates();

    FFunctionIdentifier getFreshLambdaName();

    @Override
    String toString();

    @Override
    default boolean canImplicitlyCast() {
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

    default void addDelegate(FField field) throws DelegateFromTypeVar {
        assert field.getMemberOf() == this;
        if (!(field.getType() instanceof FClass))
            throw new DelegateFromTypeVar(field);
        if (field.getVisibility() != FVisibilityModifier.PRIVATE)
            getDirectDelegates().put(field.getType(), field);
    }

    default Pair<FField, ImplicitTypeCast> getDelegate(FType toType, Multimap<FTypeVariable, TypeConstraint> constraints) {
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

    @Override
    default FField getField(FIdentifier identifier) throws FieldNotFound {
        FField f = getInstanceFields().get(identifier);
        if (f != null)
            return f;
        f = getStaticFields().get(identifier);
        if (f != null)
            return f;
        throw new FieldNotFound(identifier);
    }

    default Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    default void addField(FField field) throws IdentifierCollision {
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
    }

    default void addFieldTrusted(FField field) {
        try {
            addField(field);
        } catch (IdentifierCollision identifierCollision) {
            Utils.cantHappen();
        }
    }

    default void addFunction(FFunction function) throws SignatureCollision {
        for (FFunction other : getFunctions().get(function.getIdentifier())) {
            if (SignatureCollision.collide(function, other))
                throw new SignatureCollision(function, other);
        }
        getFunctions().put(function.getIdentifier(), function);
    }

    default void addFunctionTrusted(FFunction function) {
        try {
            addFunction(function);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    default FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getFunctions().get(FConstructor.IDENTIFIER));
    }

    default FConstructor generateConstructor() {
        FVisibilityModifier visibility = getConstructorVisibility() == null ? FVisibilityModifier.PRIVATE : getConstructorVisibility();
        try {
            addFunction(FConstructor.createMalloc(this));
            FConstructor res = FConstructor.create(visibility, this);
            addFunction(res);
            return res;
        } catch (SignatureCollision signatureCollision) {
            return Utils.handleException(signatureCollision);
        }
    }

    default void removeUnreachable(Reachability.ReachableClass reachable) {
        getStaticFields().values().retainAll(reachable.reachableFields);
        getInstanceFields().values().retainAll(reachable.reachableFields);
        getFunctions().values().retainAll(reachable.reachableFunctions.keySet());
    }

    default FFunction resolveFunction(FFunctionIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, Multimap<FTypeVariable, TypeConstraint> constraints) throws FunctionNotFound {
        Collection<FFunction> candidates = getFunctions().get(identifier);
        FunctionResolver.Result result = FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, candidates);
        constraints.putAll(result.constraints);
        return result.function;
    }

    default  <C,Fi,Fu,S,E> C accept(ClassVisitor<C, Fi, Fu, S, E> visitor) {
        visitor.enterType(this);
        List<Fi> fields = new ArrayList<>(this.getInstanceFields().size() + this.getStaticFields().size());
        for (FField f : this.getInstanceFields().values()) {
            fields.add(f.accept(visitor));
        }
        for (FField f : this.getStaticFields().values()) {
            fields.add(f.accept(visitor));
        }
        List<Fu> functions = new ArrayList<>(this.getFunctions().size());
        for (FFunction f : this.getFunctions().values()) {
            functions.add(f.accept(visitor));
        }
        return visitor.exitType(this, fields, functions);
    }

    @Override
    default StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }

    default String headerToString() {
        return getVisibility() + " class " + getIdentifier();
    }

    default StringBuilder summary(StringBuilder sb) {
        sb.append(headerToString()).append("{\n  ");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append(", ");
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append(", ");
        }
        sb.append("\n  ");
        for (FFunction function : getFunctions().values()) {
            sb.append(function.headerToString()).append(", ");
        }
        return sb.append("\n}");
    }

    default StringBuilder printAll(StringBuilder sb) {
        sb.append(headerToString()).append("{\n");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FFunction function : getFunctions().values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }
}
