package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;

import java.util.Map;

public class DummyFunction extends FBaseFunction {

    public DummyFunction(FIdentifier identifier, Namespace memberOf, FVisibilityModifier modifier, boolean natiwe, FType returnType, ImmutableList<FParameter> params, ImmutableList<FParameter> assignees, Map<FIdentifier, FTypeVariable> parameters) {
        super(identifier, memberOf, modifier, natiwe, false, returnType, params, assignees, parameters);
    }
}
