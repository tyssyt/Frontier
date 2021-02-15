package tys.frontier.code.type;

import com.google.common.collect.BiMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Multimap;
import com.google.common.collect.UnmodifiableIterator;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.expression.DynamicFunctionCall;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FExpressionStatement;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public abstract class FClass implements FType {
    public abstract void setParameters(List<FTypeVariable> parameters, List<Variance> parameterVariance);

    public abstract FVisibilityModifier getConstructorVisibility();

    public abstract void setConstructorVisibility(FVisibilityModifier constructorVisibility);

    public abstract BiMap<FIdentifier, InstanceField> getInstanceFields();

    public abstract List<? extends FType> getParametersList();

    public abstract Variance getParameterVariance(FTypeVariable parameter);

    public abstract Variance getParameterVariance(int i);

    public abstract FClass getInstantiation(List<FType> types) throws WrongNumberOfTypeArguments, NonEmbeddableType;

    public abstract Map<FType, FField> getDirectDelegates();

    public abstract void setForImpl(ForImpl forImpl);

    public abstract boolean isPredefined();

    @Override
    public abstract DefaultNamespace getNamespace();

    @Override
    public FIdentifier getIdentifier() {
        return getNamespace().getIdentifier();
    }

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

    public void addDelegate(InstanceField field) throws DelegateFromTypeVar {
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

    public void addField(InstanceField field) throws IdentifierCollision, SignatureCollision {
        FField old = getInstanceFields().put(field.getIdentifier(), field);
        if (old != null) {
            throw new IdentifierCollision(field, old);
        }
        getNamespace().addFunction(field.getGetter());
        getNamespace().addFunction(field.getSetter());

        if (field.getType() instanceof FFunctionType) { //TODO similar code in DefaultNamespace
            FFunctionType functionType = (FFunctionType) field.getType();
            List<FType> params = new ArrayList<>();
            params.add(this);
            params.addAll(FTuple.unpackType(functionType.getIn()));
            FBaseFunction dynamicCall = new FunctionBuilder(field.getIdentifier(), getNamespace())
                    .setParams(params).setReturnType(functionType.getOut())
                    .setLocation(field.getLocation()).setVisibility(field.getVisibility()).build();

            //TODO @PositionForGeneratedCode
            UnmodifiableIterator<FParameter> it = dynamicCall.getSignature().getParameters().iterator();
            FVariableExpression thisVar = new FVariableExpression(null, it.next());
            List<FExpression> paramExprs = new ArrayList<>(dynamicCall.getSignature().getParameters().size() - 1);
            while (it.hasNext())
                paramExprs.add(new FVariableExpression(null, it.next()));

            FFunctionCall fieldGet = FFunctionCall.createTrusted(null, field.getGetter().getSignature(), List.of(thisVar));
            DynamicFunctionCall call = DynamicFunctionCall.createTrusted(null, fieldGet, paramExprs);
            FStatement body = functionType.getOut() == FTuple.VOID ?
                    new FExpressionStatement(null, call) :
                    FReturn.createTrusted(null, List.of(call), dynamicCall);
            dynamicCall.setBody(FBlock.from(null, body));
            getNamespace().addFunction(dynamicCall);
        }
    }

    public void addFieldTrusted(InstanceField field) {
        try {
            addField(field);
        } catch (IdentifierCollision | SignatureCollision collision) {
            Utils.cantHappen();
        }
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getNamespace().getFunctions(false).get(FConstructor.NEW_ID)).getFunction();
    }

    public FConstructor generateConstructor(boolean predefined) {
        FVisibilityModifier visibility = getConstructorVisibility() == null ? FVisibilityModifier.PRIVATE : getConstructorVisibility();
        try {
            if (!predefined)
                getNamespace().addFunction(FConstructor.createMalloc(this));
            FConstructor res = FConstructor.create(visibility, this, predefined);
            getNamespace().addFunction(res);
            return res;
        } catch (SignatureCollision signatureCollision) {
            return Utils.handleException(signatureCollision);
        }
    }

    public <N,C,Fi,Fu,S,E> C accept(ClassVisitor<N, C, Fi, Fu, S, E> visitor) {
        visitor.enterClass(this);
        List<Fi> instanceFields = new ArrayList<>(this.getInstanceFields().size());
        for (FField f : this.getInstanceFields().values())
            instanceFields.add(f.accept(visitor));
        return visitor.exitClass(this, instanceFields);
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
