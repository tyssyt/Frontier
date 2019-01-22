package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class FField extends FVariable implements FTypeMember, StringBuilderToString {
    private FClass memberOf;
    private FVisibilityModifier visibility;
    private boolean statik;
    private boolean hasAssignment;
    private FExpression assignment;

    private FLocalVariable _this; //TODO this is needed for instance fields but will likely no longer be necessary if we do attributes

    private List<FFieldAccess> accessedBy = new ArrayList<>();

    public FField(FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean statik, boolean hasAssignment) {
        super(identifier, type);
        this.memberOf = memberOf;
        this.visibility = visibility;
        this.statik = statik;
        this.hasAssignment = hasAssignment;
        this._this = new FLocalVariable(FVariableIdentifier.THIS, memberOf);
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public boolean isInstance() {
        return !statik;
    }

    public FLocalVariable getThis() {
        return _this;
    }

    @Override
    public FClass getMemberOf() {
        return memberOf;
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.FIELD;
    }

    public boolean hasAssignment() {
        return hasAssignment;
    }

    public void setAssignment(FExpression assignment) throws IncompatibleTypes {
        assert this.assignment == null && hasAssignment;
        this.assignment = assignment.typeCheck(getType());
    }

    public void setAssignmentTrusted(FExpression assignment) {
        try {
            setAssignment(assignment);
        } catch (IncompatibleTypes incompatibleTypes) {
            Utils.cantHappen();
        }
    }

    public Optional<FExpression> getAssignment() {
        return Optional.ofNullable(assignment);
    }

    public boolean addAccess(FFieldAccess fieldAccess) {
        return accessedBy.add(fieldAccess);
    }

    public List<FFieldAccess> getAccessedBy() {
        return accessedBy;
    }

    public <C,Fi,Fu,S,E> Fi accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterField(this);
        return visitor.exitField(this, getAssignment().map(assignment -> assignment.accept(visitor)));
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(visibility).append(' ');
        if (statik)
            sb.append("static ");
        sb.append(super.toString());
        //noinspection ResultOfMethodCallIgnored
        getAssignment().ifPresent(a -> a.toString(sb.append(" = ")));
        return sb.append(";");
    }
    @Override
    public String toString() {
        return tS();
    }
}
