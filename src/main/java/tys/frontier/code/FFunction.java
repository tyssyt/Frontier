package tys.frontier.code;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.MapMaker;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.identifier.*;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.*;

//TODO the FFunction hierachy is so messy with constructors and predefined and stuff...
public class FFunction implements FTypeMember, HasTypeParameters<FFunction>, IdentifierNameable, Typed, ControlFlowIDontKnow, StringBuilderToString {

    private FFunctionIdentifier identifier;
    private FType memberOf;
    private FVisibilityModifier modifier;
    private List<FFunctionCall> calledBy = new ArrayList<>();
    private boolean natiwe;
    private FType returnType;
    private ImmutableList<FParameter> params;
    private Signature signature;
    protected FBlock body;

    private Map<FTypeIdentifier, FTypeVariable> parameters;
    private List<FTypeVariable> parametersList;
    private Map<TypeInstantiation, FInstantiatedFunction> instantiations;

    protected boolean predefined = false;

    public FFunction(FFunctionIdentifier identifier, FType memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params) {
        this(identifier, memberOf, modifier, natiwe, returnType, params, Collections.emptyMap());
    }

    public FFunction(FFunctionIdentifier identifier, FType memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        this.identifier = identifier;
        this.memberOf = memberOf;
        this.modifier = modifier;
        this.natiwe = natiwe;
        this.returnType = returnType;
        this.params = params;
        this.signature = new Signature(this);
        if (parameters.isEmpty()) {
            this.parameters = Collections.emptyMap();
            this.parametersList = Collections.emptyList();
            this.instantiations = Collections.emptyMap();
        } else {
            this.parameters = parameters;
            this.parametersList = new ArrayList<>(parameters.values());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();
        }
    }

    @Override
    public boolean isInstance() {
        return params.size() > 0 && params.get(0).getType() == memberOf && !params.get(0).hasDefaultValue();
    }

    @Override
    public FType getMemberOf() {
        return memberOf;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return modifier;
    }

    public boolean isNative() {
        return natiwe;
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
    public FType getType() {
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
                && modifier == FVisibilityModifier.EXPORT
                && ((FClass) memberOf).getVisibility() == FVisibilityModifier.EXPORT
                && returnType == FVoid.INSTANCE;
    }

    private NameGenerator freshVariableNames = new NameGenerator("?", "");
    public FLocalVariable getFreshVariable(FType type) {
        String name = freshVariableNames.next();
        FIdentifier identifier = type == FTypeType.INSTANCE ? new FTypeIdentifier(name) : new FVariableIdentifier(name);
        //TODO maybe be tryhards and try to find good names? like using the type as prefix?
        return new FLocalVariable(identifier, type);
    }


    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return parameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return parametersList;
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        //TODO
        return null;
        /*
        TypeInstantiation intersected = typeInstantiation.intersect(parametersList);
        if (intersected.isEmpty())
            return this;
        return instantiations.computeIfAbsent(typeInstantiation, i -> new FInstantiatedClass(this, intersected));
        */
    }

    /**
     * Compares if the source signature could call a method with this signature
     * @param arguments arguments calling a function of this signature
     * @return a pair of ints, the first being the number of arguments that had to be casted, and the casting costs (bigger is worse, can be negative)
     * @throws IncompatibleSignatures when the number of parameters differs
     * @throws IncompatibleTypes when a parameter can't be casted
     */
    public IntIntPair castSignatureFrom(List<FExpression> arguments, TypeInstantiation typeInstantiation) throws IncompatibleSignatures, IncompatibleTypes { //TODO we can move this to sig again
        if (arguments.size() > params.size()) {
            throw new IncompatibleSignatures(this.getSignature(), Utils.typesFromExpressionList(arguments));
        } else if (arguments.size() < params.size()) {
            //the missing arguments might have default values, in which case the call is still valid
            if (!params.get(arguments.size()).hasDefaultValue())
                throw new IncompatibleSignatures(this.getSignature(), Utils.typesFromExpressionList(arguments));
        }

        int casts = 0;
        int cost = 0;
        for (int i = 0; i < arguments.size(); i++) {
            FExpression argument = arguments.get(i);
            FType targetType = typeInstantiation.getType(params.get(i).getType());

            FExpression cast = argument.typeCheck(targetType);
            if (cast instanceof FLiteralExpression) {
                cost -= ((FLiteralExpression) argument).distance(((FLiteralExpression) cast));
            } else if (cast instanceof FImplicitCast) {
                casts++;
                cost += 100* ((FImplicitCast) cast).getCost();
            }
        }
        return new IntIntPair(casts, cost);
    }

    public <C,Fi,Fu,S,E> Fu accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterFunction(this);
        return visitor.exitFunction(this, getBody().map(body -> body.accept(visitor)));
    }

    public String headerToString() {
        return (body==null ? "abstract " : "") + modifier + " " +  returnType.getIdentifier() + " " +identifier + " " + params;
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
        private List<FType> paramTypes = new ArrayList<>();
        private List<FType> optionalTypes = new ArrayList<>();
        private List<FType> allTypes = new ArrayList<>();

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

        public List<FType> getParamTypes() {
            return paramTypes;
        }

        public List<FType> getOptionalTypes() {
            return optionalTypes;
        }

        public List<FType> getAllParamTypes() {
            return allTypes;
        }

        public boolean isMain() {
            return identifier.name.equals("main") && paramTypes.isEmpty() && optionalTypes.isEmpty();
        }

        public boolean collidesWith(Signature other) {
            int maxNeeded = Math.max(
                     this.getParamTypes().size(),
                    other.getParamTypes().size()
            );
            int minTotal = Math.min(
                     this.getAllParamTypes().size(),
                    other.getAllParamTypes().size()
            );
            //if the length if needed arguments of one function is longer then the length of all args of the other, they can't collide
            if (minTotal < maxNeeded)
                return false;
            //otherwise try to find a arg that is different
            for (int i=0; i< minTotal; i++) {
                FType t =  this.getAllParamTypes().get(i);
                FType o = other.getAllParamTypes().get(i);
                //args are not different if they are a type variable
                if (!(t instanceof FTypeVariable) && !(o instanceof FTypeVariable) && t != o)
                    return false;
            }
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
            for (FType type : paramTypes) {
                if (first)
                    first = false;
                else
                    sb.append(',');
                sb.append(type.getIdentifier());
            }
            for (FType type : optionalTypes) {
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
        public final ImmutableList<FType> callingTypes;

        public IncompatibleSignatures(Signature functionSignature, List<FType> callingTypes) {
            super("Function " + functionSignature + " can't be called from " + callingTypes);
            this.functionSignature = functionSignature;
            this.callingTypes = ImmutableList.copyOf(callingTypes);
        }
    }
}
