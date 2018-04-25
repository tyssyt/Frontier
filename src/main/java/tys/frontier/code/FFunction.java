package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FImplicitCast;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.FStatement;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;

//TODO the FFunction hierachy is so messy with constructors and predefined and stuff...
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

    public boolean isPredefined() {
        return predefined;
    }

    public Signature getSignature() {
        return signature;
    }

    public boolean isMain() {
        return signature.isMain()
                && statik
                && modifier == FVisibilityModifier.PUBLIC
                && clazz.getVisibility() == FVisibilityModifier.PUBLIC
                && returnType == FVoid.INSTANCE;
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

    public static class Signature implements StringBuilderToString {
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

        public boolean isMain() {
            return identifier.name.equals("main") && paramTypes.isEmpty();
        }

        /**
         * Compares if the source signature could call a method with this signature
         * @param source types that want to call this signature
         * @return an array the same size as the number of parameters. True means the parameter must be cast, false that it must be casted first
         * @throws IncompatibleSignatures when the number of parameters differs
         * @throws IncompatibleTypes when a parameter can't be casted
         */
        public boolean[] castSignatureFrom(Signature source) throws IncompatibleSignatures, IncompatibleTypes {
            if (source.paramTypes.size() != paramTypes.size())
                throw new IncompatibleSignatures(this, source);
            boolean[] res = new boolean[paramTypes.size()];
            for (int i = 0; i < paramTypes.size(); i++) {
                FClass sourceType = source.paramTypes.get(i);
                FClass targetType = paramTypes.get(i);
                if (sourceType == targetType)
                    res[i] = false;
                else {
                    FImplicitCast.getCastType(targetType, sourceType);
                    res[i] = true;
                }
            }
            return res;
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
        public StringBuilder toString(StringBuilder sb) {
            sb.append(identifier).append('(');
            boolean first = true;
            for (FClass type : paramTypes) {
                if (first)
                    first = false;
                else
                    sb.append(',');
                sb.append(type.getIdentifier());
            }
            return sb.append(')');
        }

        @Override
        public String toString() {
            return tS();
        }
    }

    public static class IncompatibleSignatures extends Exception {
        public final Signature one, two;

        public IncompatibleSignatures(Signature one, Signature two) {
            super("Signatures " + one + " & " + two + " are not compatible.");
            this.one = one;
            this.two = two;
        }
    }
}
