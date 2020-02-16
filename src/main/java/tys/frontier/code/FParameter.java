package tys.frontier.code;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Set;

public class FParameter extends FLocalVariable {
    private int index = -1;
    private boolean hasDefaultValue;
    private FExpression defaultValue;
    private Set<FParameter> defaultValueDependencies;

    private FParameter(FIdentifier identifier, FType type, boolean hasDefaultValue) {
        super(identifier, type);
        this.hasDefaultValue = hasDefaultValue;
    }

    private FParameter(FIdentifier identifier, FType type, FExpression defaultValue, Set<FParameter> defaultValueDependencies) throws IncompatibleTypes {
        super(identifier, type);
        if (defaultValue != null) {
            this.hasDefaultValue = true;
            this.defaultValue = defaultValue.typeCheck(type);
            this.defaultValueDependencies = defaultValueDependencies;
        }
    }

    public static FParameter create(FIdentifier identifier, FType type, boolean hasDefaultValue) {
        return new FParameter(identifier, type, hasDefaultValue);
    }

    public static FParameter create(FIdentifier identifier, FType type, FExpression defaultValue, Set<FParameter> defaultValueDependencies) throws IncompatibleTypes {
        return new FParameter(identifier, type, defaultValue, defaultValueDependencies);
    }

    public static FParameter createTrusted(FIdentifier identifier, FType type, FExpression defaultValue, Set<FParameter> defaultValueDependencies) {
        try {
            return create(identifier, type, defaultValue, defaultValueDependencies);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        assert this.index == -1 || this.index == index;
        this.index = index;
    }

    public boolean hasDefaultValue() {
        return hasDefaultValue;
    }

    public FExpression getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(FExpression defaultValue, Set<FParameter> defaultValueDependencies) throws IncompatibleTypes {
        assert this.defaultValue == null && hasDefaultValue;
        this.defaultValue = defaultValue.typeCheck(getType());
        this.defaultValueDependencies = defaultValueDependencies;
    }

    public Set<FParameter> getDefaultValueDependencies() {
        return defaultValueDependencies;
    }

    public void setDefaultValueTrusted(FExpression defaultValue, Set<FParameter> defaultValueDependencies) {
        try {
            setDefaultValue(defaultValue, defaultValueDependencies);
        } catch (IncompatibleTypes incompatibleTypes) {
            Utils.cantHappen();
        }
    }
}