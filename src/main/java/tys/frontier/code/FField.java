package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.Optional;

public abstract class FField extends FVariable implements StringBuilderToString {
    private FVisibilityModifier visibility;
    private boolean hasAssignment;
    private FExpression assignment;
    private Location location;

    protected FieldAccessor getter;
    protected FieldAccessor setter;

    public FField(Location location, FIdentifier identifier, FType type, FVisibilityModifier visibility, boolean hasAssignment) {
        super(identifier, type);
        this.location = location;
        this.visibility = visibility;
        this.hasAssignment = hasAssignment;
    }

    abstract public DefaultNamespace getNamespace();

    public Location getLocation() {
        return location;
    }

    public FieldAccessor getGetter() {
        return getter;
    }

    public FieldAccessor getSetter() {
        return setter;
    }

    public FVisibilityModifier getVisibility() {
        return visibility;
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

    public <N,C,Fi,Fu,S,E> Fi accept(ClassVisitor<N,C,Fi,Fu,S,E> visitor) {
        visitor.enterField(this);
        return visitor.exitField(this, getAssignment().map(assignment -> assignment.accept(visitor)));
    }

    @Override
    public String toString() {
        return tS();
    }
}
