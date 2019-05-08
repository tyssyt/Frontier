package tys.frontier.code.literal;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;

import java.util.Map;

public class FLambda extends FBaseFunction {
    private FLambda(FFunctionIdentifier identifier, FType memberOf, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        super(identifier, memberOf, FVisibilityModifier.NONE, false, returnType, params, parameters);
    }

    public static FLambda create(FFunctionIdentifier identifier, FType memberOf, FType returnType, ImmutableList<FParameter> params, Map<FTypeIdentifier, FTypeVariable> parameters) {
        return new FLambda(identifier, memberOf, returnType, params, parameters);
    }
}
