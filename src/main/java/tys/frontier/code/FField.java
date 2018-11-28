package tys.frontier.code;

import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class FField extends FVariable implements FTypeMember, StringBuilderToString {
    private FClass memberOf;
    private FVisibilityModifier visibility;
    private boolean statik;
    private FVarAssignment assignment; //TODO this assignment should not be part of the field, but part of an implicit static initializer block or some similar concept

    private List<FFieldAccess> accessedBy = new ArrayList<>();

    public FField(FIdentifier identifier, FType type, FClass memberOf, FVisibilityModifier visibility, boolean statik) {
        super(identifier, type);
        assert type != FTypeType.INSTANCE;
        this.memberOf = memberOf;
        this.visibility = visibility;
        this.statik = statik;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public boolean isStatic() {
        return statik;
    }

    @Override
    public FClass getMemberOf() {
        return memberOf;
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.FIELD;
    }

    public void setAssignment(FVarAssignment assignment) {
        assert this.assignment == null;
        this.assignment = assignment;
    }

    //TODO move the assignments to initializer collection in class
    public Optional<FVarAssignment> getAssignment() {
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
        getAssignment().ifPresent(a -> a.getValue().toString(sb.append(" = ")));
        return sb.append(";");
    }
    @Override
    public String toString() {
        return tS();
    }
}
