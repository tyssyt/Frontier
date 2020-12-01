package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.*;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.location.Location;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;

public class FieldAccessor implements FFunction {

    private FField field;
    private FIdentifier identifier;
    private Signature signature;
    private Signature lhsSignature;

    private FieldAccessor(FField field, ImmutableList<FParameter> parameters, ImmutableList<FParameter> assignees, FType returnType) {
        this.field = field;
        this.identifier = field.getIdentifier();
        Pair<Signature, Signature> signatures = Signature.createSignatures(this, parameters, assignees, returnType);
        this.signature = signatures.a;
        this.lhsSignature = signatures.b;
    }

    public static Pair<FieldAccessor, FieldAccessor> createAccessors(FField field) {
        ImmutableList<FParameter> parameters;
        if (field.isInstance())
            parameters = ImmutableList.of(FParameter.create(FIdentifier.THIS, field.getMemberOf(), false));
        else
            parameters = ImmutableList.of();
        ImmutableList<FParameter> assignees = ImmutableList.of(FParameter.create(new FIdentifier("value"), field.getType(), false));

        return new Pair<>(
                new FieldAccessor(field, parameters, null, field.getType()), //getter
                new FieldAccessor(field, parameters, assignees, FTuple.VOID) //setter
        );
    }

    @Override
    public Location getLocation() {
        return new Location(field.getMemberOf().getNamespace().getLocation().getFile(), field.getPosition());
    }

    public FField getField() {
        return field;
    }

    public boolean isGetter() {
        return lhsSignature == null;
    }

    @Override
    public boolean isInstance() {
        return field.isInstance();
    }

    @Override
    public DefaultNamespace getMemberOf() {
        return field.getMemberOf().getNamespace();
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return field.getVisibility();
    }

    @Override
    public NativeDecl getNative() {
        return null;
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
    public Optional<FBlock> getBody() {
        return Optional.empty();
    }

    @Override
    public void setBody(FBlock body) {
        Utils.cantHappen();
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FType getType() {
        return null;
    }

    @Override
    public boolean isConstructor() {
        return false;
    }

    @Override
    public boolean isPredefined() {
        return true;
    }

    @Override
    public boolean isMain() {
        return false;
    }

    @Override
    public FLocalVariable getFreshVariable(FType type) {
        return Utils.cantHappen();
    }

    @Override
    public Map<FIdentifier, FTypeVariable> getParameters() {
        return emptyMap();
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return emptyList();
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
        return this;
    }
}
