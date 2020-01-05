package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.AttributeIdentifier;
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

    ListMultimap<FIdentifier, Signature> getFunctions(boolean lhsSignatures);

    List<? extends FType> getParametersList();

    Variance getParameterVariance(FTypeVariable parameter);

    Variance getParameterVariance(int i);

    FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments;

    Map<FType, FField> getDirectDelegates();

    AttributeIdentifier getFreshLambdaName();

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

    default Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    default void addField(FField field) throws IdentifierCollision, SignatureCollision {
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
        addFunction(field.getGetter());
        addFunction(field.getSetter());
    }

    default void addFieldTrusted(FField field) {
        try {
            addField(field);
        } catch (IdentifierCollision | SignatureCollision collision) {
            Utils.cantHappen();
        }
    }

    default void addFunction(FFunction function) throws SignatureCollision {
        addFunction(function.getSignature(), false);
        if (function.getLhsSignature() != null)
            addFunction(function.getLhsSignature(), true);
    }

    //TODO this is supposed to be private, but Java sucks
    default void addFunction(Signature signature, boolean lhs) throws SignatureCollision {
        for (Signature other : getFunctions(lhs).get(signature.getFunction().getIdentifier())) {
            if (SignatureCollision.collide(signature, other))
                throw new SignatureCollision(signature, other);
        }
        getFunctions(lhs).put(signature.getFunction().getIdentifier(), signature);
    }

    default void addFunctionTrusted(FFunction function) {
        try {
            addFunction(function);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    default FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getFunctions(false).get(FConstructor.IDENTIFIER)).getFunction();
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
        getStaticFields().values().removeIf(f -> !reachable.isReachable(f));
        getInstanceFields().values().removeIf(f -> !reachable.isReachable(f));
        getFunctions(false).values().removeIf(s -> !reachable.isReachable(s.getFunction()));
        getFunctions(true).clear(); //not needed after this point
    }

    @Override
    default FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        assert returnType == null || !lhsResolve;
        return FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, getFunctions(lhsResolve).get(identifier));
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
        List<Fu> functions = new ArrayList<>(this.getFunctions(false).size());
        for (Signature s : this.getFunctions(false).values()) {
            functions.add(s.getFunction().accept(visitor));
        }
        return visitor.exitType(this, fields, functions);
    }

    @Override
    default StringBuilder toString(StringBuilder sb) {
        return sb.append(getIdentifier().name);
    }
}
