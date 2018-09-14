package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FImplicitCast;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

//TODO the FFunction hierachy is so messy with constructors and predefined and stuff...
public class FFunction implements FTypeMember, IdentifierNameable, Typed, ControlFlowIDontKnow, StringBuilderToString {

    private FFunctionIdentifier identifier;
    private FClass memberOf;
    private FVisibilityModifier modifier;
    private List<FFunctionCall> calledBy = new ArrayList<>();
    private boolean natiwe;
    private boolean statik;
    private FClass returnType;
    private ImmutableList<FParameter> params;
    private Signature signature;
    protected FBlock body;

    protected boolean predefined = false;

    public FFunction(FFunctionIdentifier identifier, FClass memberOf, FVisibilityModifier modifier, boolean natiwe, boolean statik, FClass returnType, ImmutableList<FParameter> params) {
        this.identifier = identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.natiwe = natiwe;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
    }

    protected FFunction(Signature signature, FClass memberOf, FVisibilityModifier modifier, boolean natiwe, boolean statik, FClass returnType, ImmutableList<FParameter> params) {
        this.identifier = signature.identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.natiwe = natiwe;
        this.statik = statik;
        this.returnType = returnType;
        this.params = params;
        this.signature = signature;
    }

    @Override
    public FClass getMemberOf() {
        return memberOf;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return modifier;
    }

    public boolean isNative() {
        return natiwe;
    }

    @Override
    public boolean isStatic() {
        return statik;
    }

    public ImmutableList<FParameter> getParams() {
        return params;
    }

    public boolean addCall(FFunctionCall call) {
        return calledBy.add(call);
    }

    public List<FFunctionCall> getCalledBy() {
        return calledBy;
    }

    public Optional<FBlock> getBody() {
        return Optional.ofNullable(body);
    }

    public void setBody(FBlock body) {
        assert !this.natiwe && !this.predefined;
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
                && memberOf.getVisibility() == FVisibilityModifier.EXPORT
                && returnType == FVoid.INSTANCE;
    }

    private NameGenerator freshVariableNames = new NameGenerator("?", "");
    public FLocalVariable getFreshVariable(FClass type) {
        //TODO maybe be tryhards and try to find good names? like using the type as prefix?
        return new FLocalVariable(new FVariableIdentifier(freshVariableNames.next()), type);
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
        return (body==null ? "abstract " : "") + modifier + (statik ? " static " : " ") + returnType.getIdentifier() + " " +identifier + " " + params;
    }

    @Override
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append(" {\n");
        if (predefined)
            sb.append("predefined");
        else if (body != null)
            for (FStatement statement : body)
                statement.toString(sb).append('\n');
        return sb.append('}');
    }

    @Override
    public String toString() {
        return tS();
    }

    public static class Signature implements StringBuilderToString { //TODO the castSignature from is implemented outside of this, and the rest prolly takes more space then it is worth?
        private FFunctionIdentifier identifier;
        private List<FClass> paramTypes = new ArrayList<>();
        private List<FClass> optionalTypes = new ArrayList<>();
        private List<FClass> allTypes = new ArrayList<>();

        public Signature(FFunction function) {
            this.identifier = function.getIdentifier();
            for (FParameter p : function.getParams()) {
                allTypes.add(p.getType());
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

        public List<FClass> getAllParamTypes() {
            return allTypes;
        }

        public boolean isMain() {
            return identifier.name.equals("main") && paramTypes.isEmpty() && optionalTypes.isEmpty();
        }

        public boolean collidesWith(Signature other) {
            int max = Math.max(
                    this.getParamTypes().size(),
                    other.getParamTypes().size()
            );
            if (this.getAllParamTypes().size() < max || other.getAllParamTypes().size() < max)
                return false;
            return this.getAllParamTypes().subList(0, max).equals(other.getAllParamTypes().subList(0, max));
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
