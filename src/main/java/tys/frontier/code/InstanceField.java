package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Location;
import tys.frontier.util.Pair;

public class InstanceField extends FField {

    private FClass memberOf;
    private FLocalVariable _this; //TODO this is needed for instance fields but will likely no longer be necessary if we do attributes

    public InstanceField(Location location, FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean hasAssignment) {
        super(location, identifier, type, visibility, hasAssignment);
        this.memberOf = memberOf;
        this._this = new FLocalVariable(null, FIdentifier.THIS, memberOf); //TODO can I just assign the parameter of the accessor to this?
        Pair<FieldAccessor, FieldAccessor> accessors = FieldAccessor.createAccessors(this, ImmutableList.of(FParameter.create(null, FIdentifier.THIS, memberOf, false)));
        this.getter = accessors.a;
        this.setter = accessors.b;
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

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getVisibility()).append(' ').append(super.toString());
        getAssignment().ifPresent(a -> a.toString(sb.append(" = ")));
        return sb.append(";");
    }

}
