package tys.frontier.code;

import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.statement.FStatement;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class FFunction implements IdentifierNameable, Typed {

    private FFunctionIdentifier identifier;
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FClass returnType;
    private List<FVariable> params;
    private List<FStatement> body;

    public FFunction(FFunctionIdentifier identifier, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, List<FVariable> params) {
        this.identifier = identifier;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
    }

    public FClass getClazz() {
        return clazz;
    }

    public FVisibilityModifier getModifier() {
        return modifier;
    }

    public boolean isStatic() {
        return statik;
    }

    public List<FVariable> getParams() {
        return params;
    }

    public List<FStatement> getBody() {
        return body;
    }

    @Override
    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FClass getType() {
        return returnType;
    }

    public boolean isConstructor() {
        return false;
    }

    public boolean hasSameSignatureAs(FFunction other) {
        if (this.returnType != other.returnType || this.params.size() != other.params.size())
            return false;
        Set<FClass> types = params.stream().map(FVariable::getType).collect(Collectors.toSet());
        return other.params.stream().map(FVariable::getType).allMatch(types::contains);
    }

    @Override
    public String toString() {
        return modifier + (statik ? " static " : " ") + returnType + " " +identifier + " " + params;
    }
}
