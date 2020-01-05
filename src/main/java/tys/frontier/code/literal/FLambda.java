package tys.frontier.code.literal;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.AttributeIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class FLambda extends FBaseFunction {
    private FLambda(AttributeIdentifier identifier, FType memberOf, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        super(identifier, memberOf, FVisibilityModifier.NONE, false, returnType, params, null, parameters);
    }

    public static FLambda create(AttributeIdentifier identifier, FType memberOf, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        return new FLambda(identifier, memberOf, returnType, params, parameters);
    }

    public void finishLambda () { //TODO the fact that this function has to exists is prove that somthing went wrong
        Map<FTypeVariable, FType> baseMap = new HashMap<>(getParameters().size());

        Iterator<FTypeVariable> it = getParametersList().iterator();
        while (it.hasNext()) {
            FTypeVariable var = it.next();
            if (var.getConstraints().isResolved()) {
                baseMap.put(var, var.getConstraints().getResolved());
                it.remove();
                parameters.remove(var.getIdentifier());
            }
        }
        TypeInstantiation typeInstantiation = TypeInstantiation.create(baseMap);
        FType newReturnType = typeInstantiation.getType(getType());
        getSignature().setType(newReturnType);
        if (getLhsSignature() != null)
            getLhsSignature().setType(newReturnType);
        for (FParameter param : getSignature().getParameters()) { //rhsSignature parameters contains all lhsSignature parameters and assignees
            param.setType(typeInstantiation.getType(param.getType())); //TODO delete public setter of Variable Type...
        }
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        //Lambda is only used with addresses, so when this is called typeInstantiation should be emtpy, and all newParameters should be instantiated
        if (typeInstantiation.isEmpty()) { //TODO this is one hell pf an unstable hack, good luck future me
            Map<FTypeVariable, FType> baseMap = new HashMap<>(getParameters().size());
            for (FTypeVariable var : getParametersList()) {
                assert var.getConstraints().isResolved();
                baseMap.put(var, var.getConstraints().getResolved());
            }
            return super.getInstantiation(TypeInstantiation.create(baseMap));
        } else
            return super.getInstantiation(typeInstantiation);
    }
}
