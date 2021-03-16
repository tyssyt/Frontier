package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.FAssignment;
import tys.frontier.code.statement.FVarDeclaration;
import tys.frontier.code.type.FType;
import tys.frontier.util.Conditions;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.function.Predicate.not;
import static tys.frontier.util.Conditions.*;
import static tys.frontier.util.Utils.mutableSingletonList;

public class OptionalInformationForIf {

    private boolean promoteThen;
    private List<FLocalVariable> promotableVars;
    private List<FExpression> lambdaValues;

    public OptionalInformationForIf( List<FExpression> lambdaValues, List<FLocalVariable> promotableVars, boolean promoteThen) {
        assert promoteThen || lambdaValues.isEmpty();
        this.promoteThen = promoteThen;
        this.promotableVars = promotableVars;
        this.lambdaValues = lambdaValues;
    }

    //the logic in here needs to stay in sync with LambdaIfLowering
    public static OptionalInformationForIf createFromCondition(FExpression condition) {
        condition = removeBrackets(condition);

        boolean negated = isBooleanNot(condition);
        if (negated)
            condition = removeBrackets(((FFunctionCall) condition).getArguments(false).get(0));

        if (!negated && isOptionalExistAtom(condition)) {
            FExpression atom = ((FImplicitCast) condition).getCastedExpression();
            if (atom instanceof FVariableExpression)
                return new OptionalInformationForIf(List.of(), List.of(((FVariableExpression) atom).getVariable()), true);
            else
                return new OptionalInformationForIf(List.of(atom), List.of(), true);
        }
        assert !(negated && isOptionalExistAtom(condition));
        if (!negated && isOptionalNotExistsAtom(condition)) {
            FExpression atom = ((FFunctionCall) condition).getArguments(false).get(0);
            if (atom instanceof FVariableExpression)
                return new OptionalInformationForIf(List.of(), List.of(((FVariableExpression) atom).getVariable()), false);
            else
                return new OptionalInformationForIf(List.of(), List.of(), false);
        }

        if (isBinaryBooleanOp(condition, BinaryOperator.AND)) {
            List<FExpression> atoms = splitOnOp(condition, BinaryOperator.AND);
            atoms.removeIf(not(Conditions::isOptionalExistAtom));
            atoms.replaceAll(atom -> ((FImplicitCast)atom).getCastedExpression());
            return createFromAtoms(atoms, !negated, !negated);
        } else if (isBinaryBooleanOp(condition, BinaryOperator.OR)) {
            List<FExpression> atoms = splitOnOp(condition, BinaryOperator.OR);
            atoms.removeIf(not(Conditions::isOptionalNotExistsAtom));
            atoms.replaceAll(atom -> ((FFunctionCall)atom).getArguments(false).get(0));
            return createFromAtoms(atoms, negated, false);
        }

        return new OptionalInformationForIf(List.of(), List.of(), true);
    }

    private static OptionalInformationForIf createFromAtoms(List<FExpression> optionalAtoms, boolean promoteThen, boolean allowLambda) {
        List<FLocalVariable> promotableVars = new ArrayList<>();
        List<FExpression> lambdaValues = allowLambda ? new ArrayList<>() : List.of();

        for (FExpression atom : optionalAtoms) {
            assert atom.getType() instanceof FOptional;
            if (atom instanceof FVariableExpression)
                promotableVars.add(((FVariableExpression) atom).getVariable());
            else if (allowLambda)
                lambdaValues.add(atom);
        }
        return new OptionalInformationForIf(lambdaValues, promotableVars, promoteThen);
    }

    public boolean isPromoteThen() {
        return promoteThen;
    }

    public List<FLocalVariable> getPromotableVars() {
        return promotableVars;
    }

    public List<FExpression> getLambdaValues() {
        return lambdaValues;
    }

    //TODO @PositionForGeneratedCode
    public FAssignment createPromotions(Map<FIdentifier, FLocalVariable> variableScope) {
        List<FExpression> lhs = new ArrayList<>(promotableVars.size());
        List<FExpression> promote = new ArrayList<>(promotableVars.size());
        for (FLocalVariable promoteable : promotableVars) {
            assert promoteable.getType() instanceof FOptional;
            FOptional opt = (FOptional) promoteable.getType();
            FLocalVariable promotedVar = new FLocalVariable(promoteable.getPosition(), promoteable.getIdentifier(), opt.getBaseType());
            lhs.add(new FVarDeclaration(promotedVar));
            promote.add(FFunctionCall.createTrusted(null, opt.getExmark().getSignature(), mutableSingletonList(new FVariableExpression(null, promoteable))));
            variableScope.put(promotedVar.getIdentifier(), promotedVar); //this should override the non promoted declaration
        }
        return FAssignment.createTrusted(null, lhs, promote);
    }

    public List<FType> getPromotedLambdaValueTypes() {
        return Utils.map(lambdaValues, v -> ((FOptional) v.getType()).getBaseType());
    }
}
