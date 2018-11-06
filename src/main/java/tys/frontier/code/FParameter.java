package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;

import java.util.Optional;

public class FParameter extends FLocalVariable {
    private boolean hasDefaultValue;
    private FExpression defaultValue; //optional

    public FParameter(FIdentifier identifier, FType type, boolean hasDefaultValue) {
        super(identifier, type);
        this.hasDefaultValue = hasDefaultValue;
    }

    public FParameter(FIdentifier identifier, FType type, FExpression defaultValue) {
        super(identifier, type);
        this.defaultValue = defaultValue;
        this.hasDefaultValue = defaultValue != null;
    }

    public boolean hasDefaultValue() {
        return hasDefaultValue;
    }

    public Optional<FExpression> getDefaultValue() {
        return Optional.ofNullable(defaultValue);
    }

    public void setDefaultValue(FExpression defaultValue) {
        assert this.defaultValue == null && hasDefaultValue;
        this.defaultValue = defaultValue;
    }
}
