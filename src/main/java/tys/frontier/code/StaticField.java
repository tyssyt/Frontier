package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FType;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Pair;

public class StaticField extends FField {

    private DefaultNamespace memberOf;

    public StaticField(Position position, FIdentifier identifier, FType type, DefaultNamespace memberOf, FVisibilityModifier visibility, boolean hasAssignment) {
        super(position, identifier, type, visibility, hasAssignment);
        this.memberOf = memberOf;
        Pair<FieldAccessor, FieldAccessor> accessors = FieldAccessor.createAccessors(this, ImmutableList.of());
        this.getter = accessors.a;
        this.setter = accessors.b;
    }

    @Override
    public DefaultNamespace getNamespace() {
        return memberOf;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(getVisibility()).append(" static ").append(super.toString());
        getAssignment().ifPresent(a -> a.toString(sb.append(" = ")));
        return sb.append(";");
    }

}
