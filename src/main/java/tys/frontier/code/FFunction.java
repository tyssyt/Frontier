package tys.frontier.code;

import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.statement.FStatement;

import java.util.ArrayList;
import java.util.List;

public class FFunction implements IdentifierNameable, Typed {

    private FFunctionIdentifier identifier;
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FClass returnType;
    private List<FLocalVariable> params;
    private Signature signature;
    protected List<FStatement> body;

    protected boolean predefined = false;

    public FFunction(FFunctionIdentifier identifier, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, List<FLocalVariable> params) {
        this.identifier = identifier;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
    }

    protected FFunction(Signature signature, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, List<FLocalVariable> params) {
        this.identifier = signature.identifier;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = signature;
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

    public List<FLocalVariable> getParams() {
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

    public Signature getSignature() {
        return signature;
    }

    @Override
    public String toString() {
        return modifier + (statik ? " static " : " ") + returnType.getIdentifier() + " " +identifier + " " + params;
    }

    public static class Signature {
        private FFunctionIdentifier identifier;
        private List<FClass> paramTypes;

        public Signature(FFunctionIdentifier identifier, List<FClass> paramTypes) {
            this.identifier = identifier;
            this.paramTypes = paramTypes;
        }

        public Signature(FFunction function) {
            this.identifier = function.getIdentifier();
            this.paramTypes = new ArrayList<>(function.getParams().size());
            for (FLocalVariable v : function.getParams())
                paramTypes.add(v.getType());
        }

        public FFunctionIdentifier getIdentifier() {
            return identifier;
        }

        public List<FClass> getParamTypes() {
            return paramTypes;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Signature signature = (Signature) o;
            return this.identifier.equals(signature.identifier)
                    && this.getParamTypes().equals(signature.paramTypes);
        }

        @Override
        public int hashCode() {
            return  31 * identifier.hashCode() + paramTypes.hashCode();
        }

        @Override
        public String toString() {
            return identifier + ", " + paramTypes;
        }
    }

}
