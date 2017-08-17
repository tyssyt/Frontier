package tys.frontier.code;

import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.statement.FVarAssignment;

import java.util.Optional;

public class FField extends FVariable {
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FVarAssignment assignment;

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

    //TODO move the assignments to initializer collection in class
    public Optional<FVarAssignment> getAssignment() {
        return assignment == null ? Optional.empty() : Optional.of(assignment);
    }

    @Override
    public String toString() {
        return modifier + (statik ? " static " : " ") + super.toString();
    }
}
