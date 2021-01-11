package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.Typed;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.util.Pair;

import java.util.List;

public class Signature implements Typed {

    private FFunction function;

    private ImmutableList<FParameter> parameters;
    private ImmutableList<FParameter> assignees;
    private FType returnType;

    public Signature(FFunction function, ImmutableList<FParameter> parameters, ImmutableList<FParameter> assignees) {
        this.function = function;
        this.parameters = parameters;
        this.assignees = assignees;
        this.returnType = FTuple.VOID;
    }

    public Signature(FFunction function, ImmutableList<FParameter> parameters, FType returnType) {
        this.function = function;
        this.parameters = parameters;
        this.returnType = returnType;
    }

    public static Pair<Signature, Signature> createSignatures(FFunction function, ImmutableList<FParameter> parameters, ImmutableList<FParameter> assignees, FType returnType) {
        if (assignees == null) {
            numberParams(parameters);
            return new Pair<>(new Signature(function, parameters, returnType), null);
        } else {
            assert returnType == FTuple.VOID;
            ImmutableList<FParameter> rhsParameters = ImmutableList.<FParameter>builder()
                    .addAll(parameters)
                    .addAll(assignees)
                    .build();
            numberParams(rhsParameters);
            return new Pair<>(
                    new Signature(function, rhsParameters, returnType),
                    new Signature(function, parameters, assignees)
            );
        }
    }

    private static void numberParams(List<FParameter> params) {
        for (int i = 0; i < params.size(); i++)
            params.get(i).setIndex(i);
    }

    public FFunction getFunction() {
        return function;
    }

    public ImmutableList<FParameter> getParameters() {
        return parameters;
    }

    public ImmutableList<FParameter> getAssignees() {
        return assignees;
    }

    @Override
    public FType getType() {
        return returnType;
    }

    public void setType(FType returnType) {
        this.returnType = returnType;
    }

    public boolean isLhs() {
        return assignees != null;
    }

    public boolean isInstance() {
        return parameters.size() > 0 && parameters.get(0).getType().getNamespace() == function.getMemberOf();
    }

    public boolean isMain() {
        return returnType == FTuple.VOID &&
                (parameters.isEmpty() || (parameters.size() == 1 && parameters.get(0).getType() == FStringLiteral.STRING_ARRAY_TYPE));
    }

    public Signature getInstantiation(TypeInstantiation typeInstantiation) {
        FFunction instantiation = function.getInstantiation(typeInstantiation);
        return isLhs() ? instantiation.getLhsSignature() : instantiation.getSignature();
    }

    @Override
    public String toString() {
        return "Signature{" +
                "Function=" + function.getIdentifier() +
                ", parameters=" + parameters +
                ", assignees=" + assignees +
                ", returnType=" + returnType +
                '}';
    }
}
