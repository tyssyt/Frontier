package tys.frontier.parser.syntaxTree;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.cast.FImplicitCast;
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

import static tys.frontier.util.Utils.mutableSingletonList;

public class OptionalInformationForIf {

    private List<FLocalVariable> promotableVars;
    private List<FExpression> lambdaValues;

    public OptionalInformationForIf(List<FLocalVariable> promotableVars, List<FExpression> lambdaValues) {
        this.promotableVars = promotableVars;
        this.lambdaValues = lambdaValues;
    }

    public static OptionalInformationForIf createFromCondition(FExpression condition) {
        List<FLocalVariable> promotableVars = new ArrayList<>();
        List<FExpression> lambdaValues = new ArrayList<>();
        for (FExpression atom : Conditions.splitOnAnd(condition)) {
            if (Conditions.isOptionalExistAtom(atom)) {
                FExpression opt = ((FImplicitCast) atom).getCastedExpression();
                if (opt instanceof FVariableExpression)
                    promotableVars.add(((FVariableExpression) opt).getVariable());
                else
                    lambdaValues.add(opt);
            }
        }
        return new OptionalInformationForIf(promotableVars, lambdaValues);
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
            FLocalVariable promotedVar = new FLocalVariable(promoteable.getIdentifier(), opt.getBaseType());
            lhs.add(new FVarDeclaration(null, promotedVar));
            promote.add(FFunctionCall.createTrusted(null, opt.getExmark().getSignature(), mutableSingletonList(new FVariableExpression(null, promoteable))));
            variableScope.put(promotedVar.getIdentifier(), promotedVar); //this should override the non promoted declaration
        }
        return FAssignment.createTrusted(null, lhs, promote);
    }

    public List<FType> getPromotedLambdaValueTypes() {
        return Utils.map(lambdaValues, v -> ((FOptional) v.getType()).getBaseType());
    }
}
