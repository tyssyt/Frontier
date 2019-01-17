package tys.frontier.parser;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import tys.frontier.code.*;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFieldAccess;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLocalVariableExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.selector.Selector;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FExpressionStatement;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.parser.syntaxErrors.CyclicDelegate;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.util.Pair;

import java.util.*;

public class Delegates {

    private Multimap<FType, Delegate> delegateToMap = ArrayListMultimap.create();

    private static class Delegate{
        FField field;
        Selector<FFunctionIdentifier> selector;
        List<Pair<FFunction, FFunction>> functions = new ArrayList<>();

        public Delegate(FField field, Selector<FFunctionIdentifier> selector) {
            this.field = field;
            this.selector = selector;
        }
    }

    public void add(FField field, Selector<FFunctionIdentifier> selector) {
        Delegate d = new Delegate(field, selector);
        FType to = field.getMemberOf();
        delegateToMap.put(to, d);
    }

    public void createDelegatedFunctions() throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();
        Set<FType> toDo = new HashSet<>(delegateToMap.keySet());

        while (!toDo.isEmpty()) {
            boolean changed = false;
            classLoop: for (Iterator<FType> it = toDo.iterator(); it.hasNext();) {
                FType cur = it.next();
                for (Delegate d : delegateToMap.get(cur)) {
                    FType from = d.field.getType();
                    if (toDo.contains(from))
                        continue classLoop; //dependency found, wait
                }
                for (Delegate d : delegateToMap.get(cur)) {
                    createDelegatedFunctions(d, errors);
                }
                it.remove();
                changed = true;
            }
            if (!changed)
                throw SyntaxErrors.create(Collections.singleton(new CyclicDelegate(null)));
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    private void createDelegatedFunctions(Delegate d, List<SyntaxError> errors) {
        if (!(d.field.getType() instanceof FClass))
            return;
        FClass from = (FClass) d.field.getType();
        FClass to = d.field.getMemberOf();
        for (Map.Entry<FFunctionIdentifier, Collection<FFunction>> entry : from.getFunctions().asMap().entrySet()) {
            if (d.selector.has(entry.getKey())) {
                for (FFunction toDelegate : entry.getValue()) {
                    if (toDelegate.getVisibility() != FVisibilityModifier.PRIVATE && toDelegate.isInstance()) {
                        //replace first param to match the class delegating to replaceing the class delegating from
                        ImmutableList<FParameter> params = toDelegate.getParams();
                        ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
                        builder.add(FParameter.create(params.get(0).getIdentifier(), to, false));
                        builder.addAll(params.subList(1, params.size()));

                        FFunction del = new FFunction(toDelegate.getIdentifier(), to, to.getVisibility(), false, toDelegate.getType(), builder.build());
                        try {
                            to.addFunction(del);
                            d.functions.add(new Pair<>(del, toDelegate));
                        } catch (SignatureCollision signatureCollision) {
                            errors.add(signatureCollision);
                        }
                    }
                }
            }
        }
    }

    public void createDelegatedFunctionBodies() {
        for (Delegate d : delegateToMap.values()) {
            createFunctionBody(d);
        }
    }

    private void createFunctionBody(Delegate d) {
        for (Pair<FFunction, FFunction> toDoPair : d.functions) {
            FFunction toDo = toDoPair.a;
            ImmutableList<FParameter> params = toDo.getParams();

            FFieldAccess fieldAccess;
            if (d.field.isInstance()) {
                FLocalVariableExpression thisExpr = new FLocalVariableExpression(params.get(0));
                fieldAccess = FFieldAccess.createInstanceTrusted(d.field, thisExpr);
            } else {
                fieldAccess = FFieldAccess.createStatic(d.field);
            }

            List<FExpression> arguments = new ArrayList<>(params.size());
            arguments.add(fieldAccess);
            for (int i = 1; i < params.size(); i++) {
                arguments.add(new FLocalVariableExpression(params.get(i)));
            }

            FFunctionCall functionCall = FFunctionCall.createTrusted(toDoPair.b, arguments);

            FStatement res;
            if (toDo.getType() == FVoid.INSTANCE)
                res = new FExpressionStatement(functionCall);
            else
                res = FReturn.createTrusted(functionCall, toDo);

            toDo.setBody(FBlock.from(res));
        }
    }

}
