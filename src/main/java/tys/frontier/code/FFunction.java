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
    private ImmutableList<FParameter> params;
    private Signature signature;
    protected ImmutableList<FStatement> body;

    protected boolean predefined = false;

    public FFunction(FFunctionIdentifier identifier, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, ImmutableList<FParameter> params) {
        this.identifier = identifier;
        this.clazz = clazz;
        this.modifier = modifier;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
    }

    protected FFunction(Signature signature, FClass clazz, FVisibilityModifier modifier, boolean statik, FClass returnType, ImmutableList<FParameter> params) {
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
    public FVisibilityModifier getVisibility() {
        return modifier;
    }

    @Override
    public boolean isStatic() {
        return statik;
    }

    public ImmutableList<FParameter> getParams() {
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
                && modifier == FVisibilityModifier.EXPORT
                && clazz.getVisibility() == FVisibilityModifier.EXPORT
                && returnType == FVoid.INSTANCE;
    }

    /**
     * Compares if the source signature could call a method with this signature
     * @param callingTypes types that want to call this signature
     * @return an array the same size as the number of parameters. True means the parameter must be cast, false that it must be casted first
     * @throws IncompatibleSignatures when the number of parameters differs
     * @throws IncompatibleTypes when a parameter can't be casted
     */
    public boolean[] castSignatureFrom(List<FClass> callingTypes) throws IncompatibleSignatures, IncompatibleTypes { //TODO we can move this to sig again
        if (callingTypes.size() > params.size()) {
            throw new IncompatibleSignatures(this.getSignature(), callingTypes);
        } else if (callingTypes.size() < params.size()) {
            //the missing arguments might have default values, in which case the call is still valid
            if (!params.get(callingTypes.size()).getDefaultValue().isPresent())
                throw new IncompatibleSignatures(this.getSignature(), callingTypes);
        }
        boolean[] res = new boolean[callingTypes.size()];
        for (int i = 0; i < callingTypes.size(); i++) {
            FClass sourceType = callingTypes.get(i);
            FClass targetType = params.get(i).getType();
            if (sourceType == targetType)
                res[i] = false;
            else {
                FImplicitCast.getCastType(targetType, sourceType);
                res[i] = true;
            }
        }
        return res;
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
        private List<FClass> optionalTypes;

        public Signature(FFunction function) {
            this.identifier = function.getIdentifier();
            this.paramTypes = new ArrayList<>();
            this.optionalTypes = new ArrayList<>();
            for (FParameter p : function.getParams()) {
                if (p.hasDefaultValue())
                    optionalTypes.add(p.getType());
                else
                    paramTypes.add(p.getType());
            }
        }

        public FFunctionIdentifier getIdentifier() {
            return identifier;
        }

        public List<FClass> getParamTypes() {
            return paramTypes;
        }

        public List<FClass> getOptionalTypes() {
            return optionalTypes;
        }

        public boolean isMain() {
            return identifier.name.equals("main") && paramTypes.isEmpty() && optionalTypes.isEmpty();
        }

        public boolean collidesWith(Signature other) {
            List<FClass> shorter;
            List<FClass> longer;
            if (this.paramTypes.size() <= other.paramTypes.size()) {
                shorter = this.paramTypes;
                longer = other.paramTypes;
            } else {
                shorter = other.paramTypes;
                longer = this.paramTypes;
            }

            if (!shorter.equals(longer.subList(0, shorter.size())))
                return false;
            //TODO there are other non-colliding scenarios
            return true;
        }

        //TODO castSig to here?

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Signature)) return false;

            Signature signature = (Signature) o;

            if (!identifier.equals(signature.identifier)) return false;
            if (!paramTypes.equals(signature.paramTypes)) return false;
            return optionalTypes.equals(signature.optionalTypes);
        }

        @Override
        public int hashCode() {
            int result = identifier.hashCode();
            result = 31 * result + paramTypes.hashCode();
            result = 31 * result + optionalTypes.hashCode();
            return result;
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
            for (FClass type : optionalTypes) {
                if (first)
                    first = false;
                else
                    sb.append(',');
                sb.append(type.getIdentifier()).append('=');
            }
            return sb.append(')');
        }

        @Override
        public String toString() {
            return tS();
        }
    }

    public static class IncompatibleSignatures extends Exception {
        public final Signature functionSignature;
        public final ImmutableList<FClass> callingTypes;

        public IncompatibleSignatures(Signature functionSignature, List<FClass> callingTypes) {
            super("Function " + functionSignature + " can't be called from " + callingTypes);
            this.functionSignature = functionSignature;
            this.callingTypes = ImmutableList.copyOf(callingTypes);
        }
    }
}
