package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

public class FParameter extends FLocalVariable {
    private boolean hasDefaultValue;
    private FExpression defaultValue;

    private FParameter(FIdentifier identifier, FType type, boolean hasDefaultValue) {
        super(identifier, type);
        this.hasDefaultValue = hasDefaultValue;
    }

    private FParameter(FIdentifier identifier, FType type, FExpression defaultValue) throws IncompatibleTypes {
        super(identifier, type);
        if (defaultValue != null) {
            this.hasDefaultValue = true;
            this.defaultValue = defaultValue.typeCheck(type);
        }
    }

    public static FParameter create(FIdentifier identifier, FType type, boolean hasDefaultValue) {
        return new FParameter(identifier, type, hasDefaultValue);
    }

    public static FParameter create(FIdentifier identifier, FType type, FExpression defaultValue) throws IncompatibleTypes {
        return new FParameter(identifier, type, defaultValue);
    }

    public static FParameter createTrusted(FIdentifier identifier, FType type, FExpression defaultValue) {
        try {
            return create(identifier, type, defaultValue);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public boolean hasDefaultValue() {
        return hasDefaultValue;
    }

    public FExpression getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(FExpression defaultValue) throws IncompatibleTypes {
        assert this.defaultValue == null && hasDefaultValue;
        this.defaultValue = defaultValue.typeCheck(getType());
    }

    public void setDefaultValueTrusted(FExpression defaultValue) {
        try {
            setDefaultValue(defaultValue);
        } catch (IncompatibleTypes incompatibleTypes) {
            Utils.cantHappen();
        }
    }
}