package tys.frontier.code.expression.cast;

import tys.frontier.code.FClass;
import tys.frontier.code.FInstantiatedClass;
import tys.frontier.code.FType;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;

public class TypeParameterCast extends FImplicitCast {

    private FImplicitCast[] casts;

    private TypeParameterCast(FType type, FExpression castedExpression, Variance variance, FImplicitCast[] casts) {
        super(type, castedExpression, variance);
        assert castedExpression.getType() instanceof FInstantiatedClass && type instanceof FInstantiatedClass;
        assert ((FInstantiatedClass) castedExpression.getType()).getBaseClass() == ((FInstantiatedClass) type).getBaseClass();
        assert casts.length == ((FClass) castedExpression.getType()).getParametersList().size();
        this.casts = casts;
    }

    public static TypeParameterCast createTTTTT(FClass type, FExpression castedExpression, Variance variance) throws IncompatibleTypes {
        //assert base classes are the same
        //get BaseClass Parameter list
        //for each baseClass parameter, instantiated with the instantiations from both sides
        //try to implicit cast from one side to the other (using the variance of the base parameter)
        //fails just propaget (unless I want decent error messages)
        //if they are equal put a null in casts, otherwise put the cast in casts
        //problem, I do not cast expressions here anymore, just types, which is not possible atm, but guess we will make it work :)
        throw new IncompatibleTypes(type, castedExpression.getType());
    }

    @Override
    public int getCost() {
        int cost = 0;
        for (FImplicitCast cast : casts) {
            if (cast != null)
                cost += cast.getCost();
        }
        return cost;
    }

    @Override
    public boolean isNoOpCast() {
        for (FImplicitCast cast : casts) {
            if (cast != null && !cast.isNoOpCast())
                return false;
        }
        return true;
    }
}
