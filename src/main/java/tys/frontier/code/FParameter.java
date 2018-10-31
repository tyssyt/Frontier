package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;

import java.util.Optional;

public class FParameter extends FLocalVariable {
    private FExpression defaultValue; //optional

    public FParameter(FIdentifier identifier, FType type) {
        super(identifier, type);
    }

    public FParameter(FIdentifier identifier, FType type, FExpression defaultValue) {
        super(identifier, type);
        this.defaultValue = defaultValue;
    }

    public boolean hasDefaultValue() {
        return defaultValue != null;
    }

    public Optional<FExpression> getDefaultValue() {
        return Optional.ofNullable(defaultValue);
    }

    public void setDefaultValue(FExpression defaultValue) {
        this.defaultValue = defaultValue;
    }
}
