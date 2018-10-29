package tys.frontier.code;

import com.google.common.collect.*;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.IntIntPair;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.*;

public class FClass implements IdentifierNameable, HasVisibility, StringBuilderToString {
    protected FTypeIdentifier identifier;
    protected FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    protected FLocalVariable thiz;
    private Set<FVariable> parameters;

    private Map<FClass, FField> delegates = new HashMap<>();
    protected BiMap<FVariableIdentifier, FField> instanceFields = HashBiMap.create();
    protected BiMap<FVariableIdentifier, FField> staticFields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> instanceFunctions = ArrayListMultimap.create();
    protected Multimap<FFunctionIdentifier, FFunction> staticFunctions = ArrayListMultimap.create();

    protected Map<FFunction, String> uniqueFunctionNames;

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        this(identifier, visibility, Collections.emptySet());
    }

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility, Set<FVariable> parameters) {
        this.identifier = identifier;
        this.visibility = visibility;
        this.parameters = parameters;
        thiz = new FLocalVariable(FVariableIdentifier.THIS, this);
    }

    protected void addDefaultFunctions() {
        try {
            addFunction(FBinaryOperator.Bool.EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS.createPredefined(this));
            addFunction(FBinaryOperator.Bool.EQUALS_ID.createPredefined(this));
            addFunction(FBinaryOperator.Bool.NOT_EQUALS_ID.createPredefined(this));
        } catch (SignatureCollision e) {
            Utils.handleException(e);
        }
    }

    @Override
    public FTypeIdentifier getIdentifier () {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    public FLocalVariable getThis() {
        return thiz;
    }

    public void addDelegate(FField field) {
        assert field.getMemberOf() == this;
        if (field.getVisibility() != FVisibilityModifier.PRIVATE)
            delegates.put(field.getType(), field);
    }

    public List<FField> getDelegate(FClass toType) {
        List<FField> res = new ArrayList<>();
        if (getDelegate(toType, res))
            return res;
        else
            return null;
    }

    private boolean getDelegate(FClass toType, List<FField> res) {
        FField f = delegates.get(toType);
        if (f != null) {
            res.add(f);
            return true;
        }

        for (Map.Entry<FClass, FField> entry : delegates.entrySet()) {
            res.add(entry.getValue());
            if (entry.getKey().getDelegate(toType, res))
                return true;
            res.remove(res.size() - 1);
        }
        return false;
    }

    public BiMap<FVariableIdentifier, FField> getInstanceFields() {
        return instanceFields;
    }

    public BiMap<FVariableIdentifier, FField> getStaticFields() {
        return staticFields;
    }

    public Iterable<FField> getFields() {
        return Iterables.concat(getInstanceFields().values(), getStaticFields().values());
    }

    public Multimap<FFunctionIdentifier, FFunction> getInstanceFunctions() {
        return instanceFunctions;
    }

    public Multimap<FFunctionIdentifier, FFunction> getStaticFunctions() {
        return staticFunctions;
    }

    public Iterable<FFunction> getFunctions() {
        return Iterables.concat(getInstanceFunctions().values(), getStaticFunctions().values());
    }

    public Pair<FFunction, IntIntPair> resolveInstanceFunction (FFunctionIdentifier identifier, List<FExpression> arguments) throws FunctionNotFound {
        return new FunctionResolver(identifier, arguments).resolve();
    }

    public Pair<FFunction, IntIntPair> resolveStaticFunction (FFunctionIdentifier identifier, List<FExpression> arguments) throws FunctionNotFound {
        return new FunctionResolver(identifier, arguments).resolveStatic();
    }

    private class FunctionResolver {
        private FFunction bestFunction;
        private IntIntPair bestCosts;

        private FFunctionIdentifier identifier;
        private List<FExpression> arguments;

        FunctionResolver(FFunctionIdentifier identifier, List<FExpression> arguments) {
            this.identifier = identifier;
            this.arguments = arguments;
        }

        Pair<FFunction, IntIntPair> resolve() throws FunctionNotFound {
            for (FFunction f : getInstanceFunctions().get(identifier)) {
                try {
                    IntIntPair cost = f.castSignatureFrom(arguments);
                    updateCost(cost, f);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, Utils.typesFromExpressionList(arguments));
            return result();
        }

        Pair<FFunction, IntIntPair> resolveStatic() throws FunctionNotFound {
            for (FFunction f : getStaticFunctions().get(identifier)) {
                try {
                    IntIntPair cost = f.castSignatureFrom(arguments);
                    updateCost(cost, f);
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, Utils.typesFromExpressionList(arguments));
            return result();
        }

        private Pair<FFunction, IntIntPair> result() {
            return new Pair<>(bestFunction, bestCosts);
        }

        private void updateCost(IntIntPair newCosts, FFunction newFunction) {
            if (bestCosts == null) {
                bestCosts = newCosts;
                bestFunction = newFunction;
                return;
            }
            int res = newCosts.compareTo(bestCosts);
            if (res < 0) {
                bestCosts = newCosts;
                bestFunction = newFunction;
            } else if (res == 0) {
                bestFunction = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
            }
        }
    }

    public void addField (FField field) throws IdentifierCollision {
        if (field.isStatic()) {
            FField old = getStaticFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        } else {
            FField old = getInstanceFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        }
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        if (function.isStatic()) {
            for (FFunction other : getStaticFunctions().get(function.getIdentifier())) {
                if (function.getSignature().collidesWith(other.getSignature()))
                    throw new SignatureCollision(function, other);
            }
            getStaticFunctions().put(function.getIdentifier(), function);
        } else {
            for (FFunction other : getInstanceFunctions().get(function.getIdentifier())) {
                if (function.getSignature().collidesWith(other.getSignature()))
                    throw new SignatureCollision(function, other);
            }
            getInstanceFunctions().put(function.getIdentifier(), function);
        }
        uniqueFunctionNames = null;
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getStaticFunctions().get(FConstructor.IDENTIFIER));
    }

    public void generateConstructor() {
        FVisibilityModifier visibility = constructorVisibility == null ? FVisibilityModifier.PRIVATE : constructorVisibility;
        try {
            addFunction(FConstructor.create(visibility, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }

    public void removeConstructor() {
        getStaticFunctions().removeAll(FConstructor.IDENTIFIER);
    }

    public Map<FFunction, String> getUniqueFunctionNames() {
        if (uniqueFunctionNames == null) {
            uniqueFunctionNames = computeUniqueFunctionNames();
        }
        return uniqueFunctionNames;
    }

    private Map<FFunction, String> computeUniqueFunctionNames() {
        Map<FFunction, String> res = new HashMap<>();
        ArrayListMultimap<FFunctionIdentifier, FFunction> allFuncs = ArrayListMultimap.create();
        allFuncs.putAll(getInstanceFunctions());
        allFuncs.putAll(getStaticFunctions());
        for (Collection<FFunction> coll : allFuncs.asMap().values()) {
            List<FFunction> list = ((List<FFunction>) coll);
            String name = list.get(0).getIdentifier().name;

            if (list.size() == 1) {
                res.put(list.get(0), name);
                continue;
            }

            list.sort((f1, f2) -> {
                int c = f1.getParams().size() - f2.getParams().size();
                if (c != 0)
                    return c;
                for (int i=0; i<f1.getParams().size(); i++) {
                    String id1 = f1.getParams().get(i).getType().getIdentifier().name;
                    String id2 = f2.getParams().get(i).getType().getIdentifier().name;
                    c = id1.compareTo(id2);
                    if (c != 0)
                        return c;
                }
                return 0;
            });
            for (int i=0; i<list.size(); i++) {
                res.put(list.get(i), name + "#" + i);
            }
        }
        return res;
    }

    public <C,Fi,Fu,S,E> C accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterType(this);
        List<Fi> fields = new ArrayList<>(this.getInstanceFields().size() + this.getStaticFields().size());
        for (FField f : this.getInstanceFields().values()) {
            visitor.enterField(f);
            fields.add(visitor.exitField(f, f.getAssignment().map(assignment -> assignment.accept(visitor))));
        }
        for (FField f : this.getStaticFields().values()) {
            visitor.enterField(f);
            fields.add(visitor.exitField(f, f.getAssignment().map(assignment -> assignment.accept(visitor))));
        }
        List<Fu> functions = new ArrayList<>(this.getInstanceFunctions().size() + this.getStaticFunctions().size());
        for (FFunction f : this.getInstanceFunctions().values()) {
            visitor.enterFunction(f);
            functions.add(visitor.exitFunction(f, f.getBody().map(body -> body.accept(visitor))));
        }
        for (FFunction f : this.getStaticFunctions().values()) {
            visitor.enterFunction(f);
            functions.add(visitor.exitFunction(f, f.getBody().map(body -> body.accept(visitor))));
        }
        return visitor.exitType(this, fields, functions);
    }

    public String headerToString() {
        return visibility + " class " + identifier;
    }

    public StringBuilder summary(StringBuilder sb) {
        sb.append(headerToString()).append("{\n  ");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append(", ");
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append(", ");
        }
        sb.append("\n  ");
        for (FFunction function : getStaticFunctions().values()) {
            sb.append(function.headerToString()).append(", ");
        }
        for (FFunction function : getInstanceFunctions().values()) {
            sb.append(function.headerToString()).append(", ");
        }
        return sb.append("\n}");
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append("{\n");
        for (FField field : getStaticFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FField field : getInstanceFields().values()) {
            field.toString(sb).append('\n');
        }
        for (FFunction function : getStaticFunctions().values()) {
            function.toString(sb).append('\n');
        }
        for (FFunction function : getInstanceFunctions().values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }

    @Override
    public String toString() {
        return tS();
    }
}
