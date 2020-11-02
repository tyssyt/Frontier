package tys.frontier.parser;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.selector.Selector;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FExpressionStatement;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.syntaxErrors.CyclicDelegate;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.util.Pair;

import java.util.*;

import static java.util.Collections.emptyList;
import static tys.frontier.util.Utils.mutableSingletonList;

public class Delegates {

    private ListMultimap<FType, Delegate> delegateToMap = MultimapBuilder.hashKeys().arrayListValues().build();

    private static class Delegate{
        FField field;
        Selector<FIdentifier> selector;
        List<Pair<FFunction, FFunction>> functions = new ArrayList<>();

        public Delegate(FField field, Selector<FIdentifier> selector) {
            this.field = field;
            this.selector = selector;
        }
    }

    public void add(FField field, Selector<FIdentifier> selector) {
        Delegate d = new Delegate(field, selector);
        FType to = field.getMemberOf();
        delegateToMap.put(to, d);
    }

    public void createDelegatedFunctions(Set<FInstantiatedClass> classesToPrepare) throws SyntaxErrors {
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
                    if (from instanceof FInstantiatedClass && classesToPrepare.remove(from))
                        ((FInstantiatedClass) from).prepare();
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
        assert d.field.getType() instanceof FClass;
        DefaultNamespace from = ((FClass) d.field.getType()).getNamespace();
        FClass to = d.field.getMemberOf();
        for (Map.Entry<FIdentifier, Collection<Signature>> entry : from.getFunctions(false).asMap().entrySet()) {
            if (d.selector.has(entry.getKey())) {
                for (Signature signature : entry.getValue()) {
                    FFunction toDelegate = signature.getFunction();
                    if (toDelegate.getVisibility() != FVisibilityModifier.PRIVATE && toDelegate.isInstance()) {
                        try {
                            d.functions.add(new Pair<>(createFunction(to, toDelegate), toDelegate));
                        } catch (SignatureCollision signatureCollision) {
                            errors.add(signatureCollision);
                        }
                    }
                }
            }
        }
    }

    private static FBaseFunction createFunction(FClass to, FFunction toDelegate) throws SignatureCollision {
        Signature signature = toDelegate.getLhsSignature() == null ? toDelegate.getSignature() : toDelegate.getLhsSignature();

        //replace first param to match the class delegating to replaceing the class delegating from
        ImmutableList<FParameter> params = signature.getParameters();
        ImmutableList.Builder<FParameter> builder = ImmutableList.builder();
        builder.add(FParameter.create(params.get(0).getIdentifier(), to, false));
        builder.addAll(params.subList(1, params.size()));
        assert toDelegate.getParameters().values().stream().allMatch(FTypeVariable::isFixed);

        DefaultNamespace namespace = to.getNamespace();
        FBaseFunction res = new FunctionBuilder(toDelegate.getIdentifier(), namespace).setVisibility(to.getVisibility())
                .setParams(builder.build()).setReturnType(signature.getType()).setAssignees(signature.getAssignees())
                .setParameters(new HashMap<>(toDelegate.getParameters())).build();
        namespace.addFunction(res);
        return res;
    }

    public void createDelegatedFunctionBodies() {
        for (Delegate d : delegateToMap.values()) {
            createFunctionBody(d);
        }
    }

    //TODO @PositionForGeneratedCode
    private void createFunctionBody(Delegate d) {
        for (Pair<FFunction, FFunction> toDoPair : d.functions) {
            FFunction toDo = toDoPair.a;
            ImmutableList<FParameter> params = toDo.getSignature().getParameters();

            FFunctionCall fieldGet;
            if (d.field.isInstance()) {
                FVariableExpression thisExpr = new FVariableExpression(null, params.get(0));
                fieldGet = FFunctionCall.createTrusted(null, d.field.getGetter().getSignature(), mutableSingletonList(thisExpr));
            } else {
                fieldGet = FFunctionCall.createTrusted(null, d.field.getGetter().getSignature(), emptyList());
            }

            List<FExpression> arguments = new ArrayList<>(params.size());
            arguments.add(fieldGet);
            for (int i = 1; i < params.size(); i++) {
                arguments.add(new FVariableExpression(null, params.get(i)));
            }

            FFunctionCall functionCall = FFunctionCall.createTrusted(null, toDoPair.b.getSignature(), arguments);

            FStatement res;
            if (toDo.getType() == FTuple.VOID)
                res = new FExpressionStatement(null, functionCall);
            else
                res = FReturn.createTrusted(null, functionCall, toDo);

            toDo.setBody(FBlock.from(res));
        }
    }

}
