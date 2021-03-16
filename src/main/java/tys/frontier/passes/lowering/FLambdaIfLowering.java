package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FCacheExpression;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import static tys.frontier.util.Conditions.*;
import static tys.frontier.util.Utils.mutableSingletonList;

public class FLambdaIfLowering extends StatementReplacer {

    private static FLambdaIfLowering INSTANCE = new FLambdaIfLowering();

    private FLambdaIfLowering() {}

    public static void lower(Module module) {
        module.accept(INSTANCE);
    }

    @Override
    public FStatement exitIf(FIf fIf, FExpression cond, FStatement then, Optional<FStatement> elze) {
        return replace(fIf);
    }

    //TODO @PositionForGeneratedCode
    //the logic in here needs to stay in sync with OptionalInformationForIf (at least the branch that can create lambdaValues)
    public FStatement replace (FIf fIf) {
        if (!(fIf.getThen() instanceof FLambdaBlock))
            return fIf;

        FLambdaBlock then = (FLambdaBlock) fIf.getThen();
        List<FLocalVariable> variables = then.getVariables();

        Iterator<FLocalVariable> varIt = variables.iterator();
        List<FExpression> newAtoms = new ArrayList<>();
        List<FExpression> castedCacheVars = new ArrayList<>(variables.size());
        for (FExpression atom : splitOnOp(removeBrackets(fIf.getCondition()), BinaryOperator.AND)) {
            if (isOptionalExistAtom(atom)) {
                FExpression castedExpression = ((FImplicitCast) atom).getCastedExpression();
                if (!(castedExpression instanceof FVariableExpression)) {
                    FCacheExpression cache = FCacheExpression.create(generateCacheName(varIt.next().getIdentifier()), castedExpression);
                    castedCacheVars.add(FFunctionCall.createTrusted(null, ((FOptional) cache.getType()).getExmark().getSignature(), mutableSingletonList(new FVariableExpression(null, cache.getVariable()))));
                    newAtoms.add(cache);
                    continue;
                }
            }

            newAtoms.add(atom);
        }
        assert !varIt.hasNext();

        //build cacheVar to var assignment
        List<FExpression> varDecls = new ArrayList<>(variables.size());
        for (FLocalVariable variable : variables)
            varDecls.add(new FVarDeclaration(variable));
        FAssignment assignment = FAssignment.createTrusted(null, varDecls, castedCacheVars);

        List<FStatement> newThen = new ArrayList<>(then.size() + 1);
        newThen.add(assignment);
        newThen.addAll(then);

        return FIf.createTrusted(fIf.getPosition(), and(newAtoms), FBlock.from(then.getPosition(), newThen), fIf.getElse().orElse(null));
    }

    private static String generateCacheName(FIdentifier identifier) {
        return '?' + identifier.name + "C";
    }
}
