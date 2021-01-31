package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Pair;

import java.util.*;

public class FBaseFunction implements FFunction {

    private FIdentifier identifier;
    private Namespace memberOf;
    private FVisibilityModifier modifier;
    private NativeDecl nativeDecl;
    protected boolean predefined;
    private FBlock body;
    private Location location;

    private Signature signature;
    private Signature lhsSignature;

    protected Map<FIdentifier, FTypeVariable> parameters;
    protected List<FTypeVariable> parametersList;
    private Map<TypeInstantiation, FInstantiatedFunction> instantiations;


    protected FBaseFunction(Location location, FIdentifier identifier, Namespace memberOf, FVisibilityModifier modifier, NativeDecl nativeDecl, boolean predefined, FType returnType, ImmutableList<FParameter> params, ImmutableList<FParameter> assignees, Map<FIdentifier, FTypeVariable> parameters) {
        this.location = location;
        this.identifier = identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.nativeDecl = nativeDecl;
        this.predefined = predefined;

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
    public Namespace getMemberOf() {
        return memberOf;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return modifier;
    }

    @Override
    public NativeDecl getNative() {
        return nativeDecl;
    }

    @Override
    public Optional<FBlock> getBody() {
        return Optional.ofNullable(body);
    }

    @Override
    public void setBody(FBlock body) {
        assert nativeDecl == null && !this.predefined;
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
                && memberOf instanceof DefaultNamespace
                && ((DefaultNamespace) memberOf).getVisibility() == FVisibilityModifier.EXPORT
                && signature.isMain();
    }

    @Override
    public Location getLocation() {
        return location;
    }

    private NameGenerator freshVariableNames = new NameGenerator("?", "");
    @Override
    public FLocalVariable getFreshVariable(Position position, FType type) {
        String name = freshVariableNames.next();
        FIdentifier identifier = new FIdentifier(name) ;
        //TODO maybe be tryhards and try to find good names? like using the type as prefix?
        return new FLocalVariable(position, identifier, type);
    }


    @Override
    public Map<FIdentifier, FTypeVariable> getParameters() {
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
