package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.cast.ImplicitTypeCast;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.TypeConstraint;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.StringBuilderToString;

import java.util.ArrayList;
import java.util.List;

public class Signature implements StringBuilderToString {

    public static class IncompatibleSignatures extends Exception {
        public final Signature functionSignature;
        public final ImmutableList<FType> callingTypes;

        public IncompatibleSignatures(Signature functionSignature, List<FType> callingTypes) {
            super("Function " + functionSignature + " can't be called from " + callingTypes);
            this.functionSignature = functionSignature;
            this.callingTypes = ImmutableList.copyOf(callingTypes);
        }
    }

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

    //TODO this can be removed and done by casting FFunctionTypes
    public IntIntPair castFrom(List<FType> arguments, TypeInstantiation typeInstantiation, Multimap<FTypeVariable, TypeConstraint> constraints) throws IncompatibleSignatures, IncompatibleTypes {
        if (arguments.size() != allTypes.size()) {
            throw new IncompatibleSignatures(this, arguments);
        }

        int casts = 0;
        int cost = 0;
        for (int i = 0; i < arguments.size(); i++) {
            FType argumentType = arguments.get(i);
            FType targetType = typeInstantiation.getType(allTypes.get(i));
            if (argumentType == targetType)
                continue;
            ImplicitTypeCast typeCast = ImplicitTypeCast.create(argumentType, targetType, Variance.Covariant, constraints);
            if (typeCast.getCost() == 0)
                continue;
            casts++;
            cost += 100*typeCast.getCost(); //the 100 is a leftover from the time we could "tighten" literals, so that those operations had a smaller weight
        }
        return new IntIntPair(casts, cost);
    }

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
