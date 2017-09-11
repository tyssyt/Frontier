package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.statement.FVarAssignment;
import tys.frontier.util.StringBuilderToString;

import java.util.Optional;

public class FField extends FVariable implements StringBuilderToString {
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FVarAssignment assignment; //TODO this assignment should not be part of the field, but part of an implicit static initializer block or some similar concept

    public FField(FVariableIdentifier identifier, FClass type, FClass clazz, FVisibilityModifier modifier, boolean statik) {
        super(identifier, type);
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
    }

    public FVisibilityModifier getModifier() {
        return modifier;
    }

    public boolean isStatic() {
        return statik;
    }

    public FClass getClazz() {
        return clazz;
    }

    public void setAssignment(FVarAssignment assignment) {
        this.assignment = assignment;
    }

    //TODO move the assignments to initializer collection in class
    public Optional<FVarAssignment> getAssignment() {
        return Optional.ofNullable(assignment);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(modifier).append(' ');
        if (statik)
            sb.append("static ");
        sb.append(super.toString());
        getAssignment().ifPresent(a -> a.getValue().toString(sb.append(" = ")));
        return sb.append(";");
    }
    @Override
    public String toString() {
        return tS();
    }
}
