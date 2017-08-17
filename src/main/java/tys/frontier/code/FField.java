package tys.frontier.code;

import tys.frontier.code.identifier.FIdentifierNameable;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.statement.FVarAssignment;

import java.util.Optional;

public class FField implements FIdentifierNameable {
    //TODO think about if this should extend FVariable instead of being a has relation
    private FVariable variable;
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FVarAssignment assignment;

    public FField(FVariable variable, FClass clazz, FVisibilityModifier modifier, boolean statik) {
        this.variable = variable;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
    }

    @Override
    public FVariableIdentifier getIdentifier() {
        return variable.getIdentifier();
    }

    public FVariable getVariable() {
        return variable;
    }

    public FVisibilityModifier getModifier() {
        return modifier;
    }

    public boolean isStatic() {
        return statik;
    }

    public Optional<FVarAssignment> getAssignment() {
        return assignment == null ? Optional.empty() : Optional.of(assignment);
    }

    @Override
    public String toString() {
        return modifier + (statik ? " static " : " ") + variable;
    }
}
