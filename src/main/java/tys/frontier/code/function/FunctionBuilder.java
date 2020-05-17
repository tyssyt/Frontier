package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Utils;

import java.util.Map;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static tys.frontier.code.FVisibilityModifier.EXPORT;
import static tys.frontier.code.predefinedClasses.FTuple.VOID;

public class FunctionBuilder {
    private FIdentifier identifier;
    private Namespace memberOf;
    private FVisibilityModifier modifier = EXPORT;
    private boolean natiwe = false;
    private boolean predefined = false;
    private FType returnType = VOID;
    private ImmutableList<FParameter> params = ImmutableList.of();
    private ImmutableList<FParameter> assignees = null;
    private Map<FIdentifier, FTypeVariable> parameters = emptyMap();

    private static final FIdentifier[] IDENTIFIERS = new FIdentifier[]{
            new FIdentifier("first"),
            new FIdentifier("second"),
            new FIdentifier("third")
    };

    public FunctionBuilder() {}

    public FunctionBuilder(FIdentifier identifier, Namespace memberOf) {
        this.identifier = identifier;
        this.memberOf = memberOf;
    }

    public FunctionBuilder setIdentifier(FIdentifier identifier) {
        this.identifier = identifier;
        return this;
    }

    public FunctionBuilder setMemberOf(Namespace memberOf) {
        this.memberOf = memberOf;
        return this;
    }

    public FunctionBuilder setVisibility(FVisibilityModifier modifier) {
        this.modifier = modifier;
        return this;
    }

    public FunctionBuilder setNative(boolean natiwe) {
        this.natiwe = natiwe;
        return this;
    }

    public FunctionBuilder setPredefined(boolean predefined) {
        this.predefined = predefined;
        return this;
    }

    public FunctionBuilder setReturnType(FType returnType) {
        assert assignees == null; // inconsistent with the behaviour of setAssignees, but that is intended
        this.returnType = returnType;
        return this;
    }

    public FunctionBuilder setParams(FType paramType) {
        this.params = ImmutableList.of(FParameter.create(FIdentifier.THIS, paramType, false));
        return this;
    }
    public FunctionBuilder setParams(FType... paramTypes) {
        ImmutableList.Builder<FParameter> builder = ImmutableList.builderWithExpectedSize(paramTypes.length);
        int i = 0;
        for (FType type : paramTypes) {
            builder.add(FParameter.create(IDENTIFIERS[i], type, false));
            i++;
        }
        this.params = builder.build();

        return this;
    }

    public FunctionBuilder setParams(ImmutableList<FParameter> params) {
        this.params = params;
        return this;
    }

    public FunctionBuilder setAssignees(ImmutableList<FParameter> assignees) { //TODO consider the same nice functions as setParams above
        if (assignees != null)
            this.returnType = VOID;
        this.assignees = assignees;
        return this;
    }

    public FunctionBuilder setParameters(FTypeVariable... parameters) {
        this.parameters = Utils.asMap(asList(parameters));
        return this;
    }

    public FunctionBuilder setParameters(Map<FIdentifier, FTypeVariable> parameters) {
        this.parameters = parameters;
        return this;
    }

    public FBaseFunction build() {
        return new FBaseFunction(identifier, memberOf, modifier, natiwe, predefined, returnType, params, assignees, parameters);
    }
}