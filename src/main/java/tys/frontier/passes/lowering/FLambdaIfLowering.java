package tys.frontier.passes.lowering;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FCacheExpression;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.expression.cast.FExplicitCast;
import tys.frontier.code.expression.cast.FImplicitCast;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FOptional;
import tys.frontier.code.statement.*;
import tys.frontier.util.Conditions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

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

    public FStatement replace (FIf fIf) {
        if (!(fIf.getThen() instanceof FLambdaBlock))
            return fIf;

        List<FLocalVariable> variables = ((FLambdaBlock) fIf.getThen()).getVariables();

        Iterator<FLocalVariable> varIt = variables.iterator();
        List<FExpression> newAtoms = new ArrayList<>();
        List<FExpression> castedCacheVars = new ArrayList<>(variables.size());
        for (FExpression atom : Conditions.splitOnAnd(fIf.getCondition())) {
            if (Conditions.isOptionalExistAtom(atom)) {
                FExpression castedExpression = ((FImplicitCast) atom).getCastedExpression();
                if (!(castedExpression instanceof FVariableExpression)) {
                    FCacheExpression cache = FCacheExpression.create(generateCacheName(varIt.next().getIdentifier()), castedExpression);
                    castedCacheVars.add(FExplicitCast.createTrusted(((FOptional) cache.getType()).getBaseType(), new FLocalVariableExpression(cache.getVariable())));
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
        FAssignment assignment = FAssignment.createTrusted(varDecls, castedCacheVars);

        List<FStatement> newThen = new ArrayList<>(fIf.getThen().size() + 1);
        newThen.add(assignment);
        newThen.addAll(fIf.getThen());

        return FIf.createTrusted(Conditions.and(newAtoms), FBlock.from(newThen), fIf.getElse().orElse(null));
    }

    private static String generateCacheName(FIdentifier identifier) {
        return '?' + identifier.name + "C";
    }
}
