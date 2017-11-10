package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.statement.FStatement;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;

public class FFunction implements FClassMember, IdentifierNameable, Typed, StringBuilderToString {

    private FFunctionIdentifier identifier;
    private FClass clazz;
    private FVisibilityModifier modifier;
    private boolean statik;
    private FClass returnType;
    private ImmutableList<FLocalVariable> params;
    private Signature signature;
    protected ImmutableList<FStatement> body;

    protected boolean predefined = false;

    public FFunction(FFunctionIdentifier identifier, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, ImmutableList<FLocalVariable> params) {
        this.identifier = identifier;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
    }

    protected FFunction(Signature signature, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, ImmutableList<FLocalVariable> params) {
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

    @Override
    public FVisibilityModifier getModifier() {
        return modifier;
    }

    @Override
    public boolean isStatic() {
        return statik;
    }

    public ImmutableList<FLocalVariable> getParams() {
        return params;
    }

    public ImmutableList<FStatement> getBody() {
        return body;
    }

    public void setBody(ImmutableList<FStatement> body) {
        assert this.body==null;
        this.body = body;
    }

    @Override
    public FFunctionIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FClass getType() {
        return returnType;
    }

    @Override
    public MemberType getMemberType() {
        return MemberType.FUNCTION;
    }

    public boolean isConstructor() {
        return false;
    }

    public Signature getSignature() {
        return signature;
    }

    public String headerToString() {
        return modifier + (statik ? " static " : " ") + returnType.getIdentifier() + " " +identifier + " " + params;
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append(" {\n");
        if (predefined)
            sb.append("predefined");
        else
            for (FStatement statement : body)
                statement.toString(sb).append('\n');
        return sb.append('}');
    }

    @Override
    public String toString() {
        return tS();
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
