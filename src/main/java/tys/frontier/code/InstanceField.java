package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.CArray;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.NonEmbeddableType;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

public class InstanceField extends FField {

    private FClass memberOf;
    private FLocalVariable _this; //TODO this is needed for instance fields but will likely no longer be necessary if we do attributes
    //TODO it is not uncommon for embedded fields (either in constructor or assignment) to create an object to then write it to the field, which will leave garbage on the heap. That should be optimized!
    private boolean embedded;

    private InstanceField(Location location, FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean hasAssignment, boolean embedded) throws NonEmbeddableType {
        super(location, identifier, type, visibility, hasAssignment);
        this.memberOf = memberOf;
        this._this = new FLocalVariable(null, FIdentifier.THIS, memberOf); //TODO can I just assign the parameter of the accessor to this?
        Pair<FieldAccessor, FieldAccessor> accessors = FieldAccessor.createAccessors(this, ImmutableList.of(FParameter.create(null, FIdentifier.THIS, memberOf, false)));
        this.getter = accessors.a;
        this.setter = accessors.b;
        this.embedded = embedded;

        if (embedded && (type instanceof FOptional || type instanceof FArray || type instanceof CArray || type instanceof FFunctionType))
            throw new NonEmbeddableType(this);
    }

    public static InstanceField create(Location location, FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean hasAssignment, boolean embedded) throws NonEmbeddableType {
        return new InstanceField(location, identifier, type, memberOf, visibility, hasAssignment, embedded);
    }

    public static InstanceField createTrusted(Location location, FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean hasAssignment, boolean embedded) {
        try {
            return create(location, identifier, type, memberOf, visibility, hasAssignment, embedded);
        } catch (NonEmbeddableType nonEmbeddableType) {
            return Utils.cantHappen();
        }
    }

    public FClass getMemberOf() {
        return memberOf;
    }

    @Override
    public DefaultNamespace getNamespace() {
        return memberOf.getNamespace();
    }

    public FLocalVariable getThis() {
        return _this;
    }

    public boolean isEmbedded() {
        return embedded;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getVisibility()).append(' ').append(super.toString());
        getAssignment().ifPresent(a -> a.toString(sb.append(" = ")));
        return sb.append(";");
    }

}
