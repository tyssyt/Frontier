package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Pair;

import java.util.*;

public class FBaseFunction implements FFunction {

    private FIdentifier identifier;
    private FType memberOf;
    private FVisibilityModifier modifier;
    private boolean natiwe;
    private FBlock body;

    private Signature signature;
    private Signature lhsSignature;

    protected Map<FTypeIdentifier, FTypeVariable> parameters;
    protected List<FTypeVariable> parametersList;
    private Map<TypeInstantiation, FInstantiatedFunction> instantiations;

    protected boolean predefined = false;

    public FBaseFunction(FIdentifier identifier, FType memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params, ImmutableList<FParameter> assignees, Map<FTypeIdentifier, FTypeVariable> parameters) {
        this.identifier = identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.natiwe = natiwe;

        Pair<Signature, Signature> pair = Signature.createSignatures(this, params, assignees, returnType);
        this.signature = pair.a;
        this.lhsSignature = pair.b;

        if (parameters.isEmpty()) {
            this.parameters = Collections.emptyMap();
            this.parametersList = Collections.emptyList();
            this.instantiations = Collections.emptyMap();
        } else {
            this.parameters = parameters;
            this.parametersList = new ArrayList<>(parameters.values());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();
        }
    }

    @Override
    public FType getMemberOf() {
        return memberOf;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return modifier;
    }

    @Override
    public boolean isNative() {
        return natiwe;
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(body);
    }

    @Override
    public void setBody(FBlock body) {
        assert !this.natiwe && !this.predefined;
        assert this.body==null;
        this.body = body;
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public boolean isConstructor() {
        return false;
    }

    @Override
    public boolean isPredefined() {
        return predefined;
    }

    @Override
    public Signature getSignature() {
        return signature;
    }

    @Override
    public Signature getLhsSignature() {
        return lhsSignature;
    }

    @Override
    public boolean isMain() {
        return identifier.name.equals("main")
                && modifier == FVisibilityModifier.EXPORT
                && ((FClass) memberOf).getVisibility() == FVisibilityModifier.EXPORT
                && signature.isMain();
    }

    private NameGenerator freshVariableNames = new NameGenerator("?", "");
    @Override
    public FLocalVariable getFreshVariable(FType type) {
        String name = freshVariableNames.next();
        FIdentifier identifier = type == FTypeType.INSTANCE ? new FTypeIdentifier(name) : new AttributeIdentifier(name);
        //TODO maybe be tryhards and try to find good names? like using the type as prefix?
        return new FLocalVariable(identifier, type);
    }


    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return parameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return parametersList;
    }

    @Override
    public boolean isInstantiation() {
        return false;
    }

    @Override
    public FFunction getBaseR() {
        return this;
    }

    @Override
    public TypeInstantiation getTypeInstantiationToBase() {
        return TypeInstantiation.EMPTY;
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        TypeInstantiation intersected = typeInstantiation.intersect(parametersList);
        if (intersected.isEmpty())
            return this;
        return instantiations.computeIfAbsent(intersected, i -> FInstantiatedFunction.fromFunctionInstantiation(this, intersected));
    }

    @Override
    public String toString() {
        return tS();
    }
}
