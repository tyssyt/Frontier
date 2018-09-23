package tys.frontier.parser;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Iterables;
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

    private Multimap<FClass, Delegate> delegateToMap = ArrayListMultimap.create();

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
        FClass to = field.getMemberOf();
        delegateToMap.put(to, d);
    }

    public void createDelegatedFunctions() throws SyntaxErrors {
        List<SyntaxError> errors = new ArrayList<>();
        Set<FClass> toDo = new HashSet<>(delegateToMap.keySet());
        while (!toDo.isEmpty()) {
            FClass cur = toDo.iterator().next();
            createDelegatedFunctions(cur, toDo, errors);
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    private void createDelegatedFunctions(FClass start, Set<FClass> toDo, List<SyntaxError> errors) throws SyntaxErrors {
        Stack<FClass> stack = new Stack<>();
        Set<FClass> cyclicCheck = new HashSet<>();
        stack.push(start);

        while (!stack.isEmpty()) {
            FClass cur = stack.peek();
            if (!toDo.contains(cur)) //we can push dependencies multiple times on the stack, remove if already handled
                continue;

            if (cyclicCheck.contains(cur)) {
                FField cause = null;
                if (delegateToMap.get(cur).size() == 1) //TODO too tired to think of how to get the field if there is more then 1...
                    cause = Iterables.getOnlyElement(delegateToMap.get(cur)).field;
                throw SyntaxErrors.create(Collections.singleton(new CyclicDelegate(cause)));
            }
            cyclicCheck.add(cur);

            //push dependencies
            boolean hasDependency=false;
            for (Delegate d : delegateToMap.get(cur)) {
                FClass from = d.field.getType();
                if (toDo.contains(from)) {
                    stack.push(from);
                    hasDependency = true;
                }
            }

            if (!hasDependency) {//actually create the delegated functions
                for (Delegate d : delegateToMap.get(cur)) {
                    createDelegatedFunctions(d, errors);
                }
                toDo.remove(cur);
                cyclicCheck.remove(cur);
                stack.pop();
            }
        }

    }

    private void createDelegatedFunctions(Delegate d, List<SyntaxError> errors) {
        FClass from = d.field.getType();
        FClass to = d.field.getMemberOf();
        for (Map.Entry<FFunctionIdentifier, Collection<FFunction>> entry : from.getInstanceFunctions().asMap().entrySet()) {
            if (d.selector.has(entry.getKey())) {
                for (FFunction toDelegate : entry.getValue()) {
                    if (toDelegate.getVisibility() != FVisibilityModifier.PRIVATE) {
                        FFunction del = new FFunction(toDelegate.getIdentifier(), to, to.getVisibility(), false, d.field.isStatic(), toDelegate.getType(), toDelegate.getParams());
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

            List<FExpression> arguments = new ArrayList<>(toDo.getParams().size()); //TODO compute size
            for (FParameter p : toDo.getParams()) {
                arguments.add(new FLocalVariableExpression(p));
            }

            FFieldAccess fieldAccess;
            if (d.field.isStatic()) {
                fieldAccess = FFieldAccess.createStatic(d.field);
            } else {
                FLocalVariableExpression thisExpr = new FLocalVariableExpression(toDo.getMemberOf().getThis());
                fieldAccess = FFieldAccess.createInstanceTrusted(d.field, thisExpr);
            }

            FFunctionCall functionCall = FFunctionCall.createInstanceTrusted(fieldAccess, toDoPair.b, arguments);

            FStatement res;
            if (toDo.getType() == FVoid.INSTANCE)
                res = new FExpressionStatement(functionCall);
            else
                res = FReturn.createTrusted(functionCall, toDo);

            toDo.setBody(FBlock.from(res));
        }
    }

}
