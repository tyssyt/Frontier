package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.NameGenerator;

import java.util.*;

public class FBaseFunction implements FFunction {

    private FFunctionIdentifier identifier;
    private FType memberOf;
    private FVisibilityModifier modifier;
    private List<FFunctionCall> calledBy = new ArrayList<>();
    private boolean natiwe;
    protected FType returnType;
    protected ImmutableList<FParameter> params;
    protected Signature signature;
    private FBlock body;

    protected Map<FTypeIdentifier, FTypeVariable> parameters;
    protected List<FTypeVariable> parametersList;
    private Map<TypeInstantiation, FInstantiatedFunction> instantiations;

    protected boolean predefined = false;

    public FBaseFunction(FFunctionIdentifier identifier, FType memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params) {
        this(identifier, memberOf, modifier, natiwe, returnType, params, Collections.emptyMap());
    }

    public FBaseFunction(FFunctionIdentifier identifier, FType memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        this.identifier = identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.natiwe = natiwe;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
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
    public ImmutableList<FParameter> getParams() {
        return params;
    }

    @Override
    public boolean addCall(FFunctionCall call) {
        return calledBy.add(call);
    }

    @Override
    public List<FFunctionCall> getCalledBy() {
        return calledBy;
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
    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FType getType() {
        return returnType;
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
    public boolean isMain() {
        return signature.isMain()
                && modifier == FVisibilityModifier.EXPORT
                && ((FClass) memberOf).getVisibility() == FVisibilityModifier.EXPORT
                && returnType == FVoid.INSTANCE;
    }

    private NameGenerator freshVariableNames = new NameGenerator("?", "");
    @Override
    public FLocalVariable getFreshVariable(FType type) {
        String name = freshVariableNames.next();
        FIdentifier identifier = type == FTypeType.INSTANCE ? new FTypeIdentifier(name) : new FVariableIdentifier(name);
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
