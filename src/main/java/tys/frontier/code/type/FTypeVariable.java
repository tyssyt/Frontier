package tys.frontier.code.type;

import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.namespace.TypeVariableNamespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.code.typeInference.ImplicitCastable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.location.Location;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;

import java.util.List;

public class FTypeVariable implements FType {

    private FIdentifier identifier;
    private List<FClass> implictCastableCovariant = List.of();
    private List<FClass> implictCastableContravariant = List.of();
    private TypeVariableNamespace namespace;

    public static FTypeVariable create(Location location, FIdentifier identifier) {
        return new FTypeVariable(location, identifier);
    }

    protected FTypeVariable(Location location, FIdentifier identifier) {
        this.identifier = identifier;
        this.namespace = new TypeVariableNamespace(location, this);
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    public List<FClass> getImplictCastableCovariant() {
        return implictCastableCovariant;
    }

    @Override
    public boolean canImplicitlyCast() {
        return true;
    }

    @Override
    public ForImpl getForImpl() {
        return null;
    }

    public void setConstraints(List<FClass> implictCastableCovariant, List<FClass> implictCastableContravariant) {
        assert this.implictCastableCovariant.isEmpty() && this.implictCastableContravariant.isEmpty();
        this.implictCastableCovariant = implictCastableCovariant;
        this.implictCastableContravariant = implictCastableContravariant;
    }

    public boolean satisfies(ImplicitCastable castable) {
        if (!(castable.getTarget() instanceof FClass))
            return false;
        FClass fClass = (FClass) castable.getTarget();

        return switch (castable.getVariance()) {
            case Covariant     -> implictCastableCovariant.contains(fClass);
            case Contravariant -> implictCastableContravariant.contains(fClass);
            case Invariant     -> false;
        };
    }

    @Override
    public Namespace getNamespace() {
        return namespace;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getIdentifier().name);
        if (!implictCastableCovariant.isEmpty() || !implictCastableContravariant.isEmpty()) {
            sb.append('[');
            if (!implictCastableCovariant.isEmpty()) {
                sb.append(Variance.Covariant._char);
                Joiners.ON_COMMA_PACKED.appendTo(sb, Utils.map(implictCastableCovariant, FClass::getIdentifier));
                if (!implictCastableContravariant.isEmpty())
                    sb.append(' ');
            }
            if (!implictCastableContravariant.isEmpty()) {
                sb.append(Variance.Contravariant._char);
                Joiners.ON_COMMA_PACKED.appendTo(sb, Utils.map(implictCastableContravariant, FClass::getIdentifier));
            }
            sb.append(']');
        }
        return sb;
    }

    @Override
    public String toString() {
        return tS();
    }
}
