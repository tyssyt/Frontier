package tys.frontier.code;

import com.google.common.collect.*;
import com.google.common.primitives.Booleans;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.*;

public class FClass implements IdentifierNameable, HasVisibility, StringBuilderToString {
    protected FTypeIdentifier identifier;
    protected FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    protected FLocalVariable thiz;

    private Map<FClass, FField> delegates = new HashMap<>();
    protected BiMap<FVariableIdentifier, FField> instanceFields = HashBiMap.create();
    protected BiMap<FVariableIdentifier, FField> staticFields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> instanceFunctions = ArrayListMultimap.create();
    protected Multimap<FFunctionIdentifier, FFunction> staticFunctions = ArrayListMultimap.create();

    protected Map<FFunction, String> uniqueFunctionNames;

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
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


    public FExpression getDefaultValue() {
        return new FLiteralExpression(FNull.INSTANCE);
    }

    public BiMap<FVariableIdentifier, FField> getInstanceFields() {
        return instanceFields;
    }

    public BiMap<FVariableIdentifier, FField> getStaticFields() {
        return staticFields;
    }

    public Iterable<FField> getFields() {
        return Iterables.concat(instanceFields.values(), staticFields.values());
    }

    public Multimap<FFunctionIdentifier, FFunction> getInstanceFunctions() {
        return instanceFunctions;
    }

    public Multimap<FFunctionIdentifier, FFunction> getStaticFunctions() {
        return staticFunctions;
    }

    public Iterable<FFunction> getFunctions() {
        return Iterables.concat(instanceFunctions.values(), staticFunctions.values());
    }

    /**
     * Resolves an instance function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * @param identifier the identifier of the function to resolve
     * @param paramTypes the parameter Types of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveInstanceFunction (FFunctionIdentifier identifier, List<FClass> paramTypes) throws FunctionNotFound {
        return new FunctionResolver(identifier, paramTypes).resolve();
    }

    /**
     * Resolves a static function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * @param identifier the identifier of the function to resolve
     * @param paramTypes the parameter Types of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveStaticFunction (FFunctionIdentifier identifier, List<FClass> paramTypes) throws FunctionNotFound {
        return new FunctionResolver(identifier, paramTypes).resolveStatic();
    }

    private class FunctionResolver {
        private int bestCost = Integer.MAX_VALUE;
        private FFunction bestFunction;
        private boolean[] bestCostArray;

        private FFunctionIdentifier identifier;
        private List<FClass> paramTypes;

        FunctionResolver(FFunctionIdentifier identifier, List<FClass> paramTypes) {
            this.identifier = identifier;
            this.paramTypes = paramTypes;
        }

        Pair<FFunction, boolean[]> resolve () throws FunctionNotFound {
            for (FFunction f : instanceFunctions.get(identifier)) {
                try {
                    boolean[] cost = f.castSignatureFrom(paramTypes);
                    if (updateCost(cost, f))
                        return result();
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, paramTypes);
            return result();
        }

        Pair<FFunction, boolean[]> resolveStatic () throws FunctionNotFound {
            for (FFunction f : staticFunctions.get(identifier)) {
                try {
                    boolean[] cost = f.castSignatureFrom(paramTypes);
                    if (updateCost(cost, f))
                        return result();
                } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
            }

            if (bestFunction == null)
                throw new FunctionNotFound(identifier, paramTypes);
            return result();
        }

        private Pair<FFunction, boolean[]> result() {
            return new Pair<>(bestFunction, bestCostArray);
        }

        private boolean updateCost(boolean[] newCostArray, FFunction newFunction) {
            int newCost = Booleans.countTrue(newCostArray);
            if (newCost < bestCost) {
                bestCost = newCost;
                bestFunction = newFunction;
                bestCostArray = newCostArray;
                if (bestCost == 0)
                    return true;
            } else if (newCost == bestCost) {
                bestFunction = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
            }
            return false;
        }
    }

    public void addField (FField field) throws IdentifierCollision {
        if (field.isStatic()) {
            FField old = staticFields.put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        } else {
            FField old = instanceFields.put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        }
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        if (function.isStatic()) {
            for (FFunction other : staticFunctions.get(function.getIdentifier())) {
                if (function.getSignature().collidesWith(other.getSignature()))
                    throw new SignatureCollision(function, other);
            }
            staticFunctions.put(function.getIdentifier(), function);
        } else {
            for (FFunction other : instanceFunctions.get(function.getIdentifier())) {
                if (function.getSignature().collidesWith(other.getSignature()))
                    throw new SignatureCollision(function, other);
            }
            instanceFunctions.put(function.getIdentifier(), function);
        }
        uniqueFunctionNames = null;
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(staticFunctions.get(FConstructor.IDENTIFIER));
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
        staticFunctions.removeAll(FConstructor.IDENTIFIER);
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
        allFuncs.putAll(instanceFunctions);
        allFuncs.putAll(staticFunctions);
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
        List<Fi> fields = new ArrayList<>(this.instanceFunctions.size() + this.staticFields.size());
        for (FField f : this.instanceFields.values()) {
            visitor.enterField(f);
            fields.add(visitor.exitField(f, f.getAssignment().map(assignment -> assignment.accept(visitor))));
        }
        for (FField f : this.staticFields.values()) {
            visitor.enterField(f);
            fields.add(visitor.exitField(f, f.getAssignment().map(assignment -> assignment.accept(visitor))));
        }
        List<Fu> functions = new ArrayList<>(this.instanceFunctions.size() + this.staticFunctions.size());
        for (FFunction f : this.instanceFunctions.values()) {
            visitor.enterFunction(f);
            functions.add(visitor.exitFunction(f, f.getBody().map(body -> body.accept(visitor))));
        }
        for (FFunction f : this.staticFunctions.values()) {
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
        for (FField field : staticFields.values()) {
            field.toString(sb).append(", ");
        }
        for (FField field : instanceFields.values()) {
            field.toString(sb).append(", ");
        }
        sb.append("\n  ");
        for (FFunction function : staticFunctions.values()) {
            sb.append(function.headerToString()).append(", ");
        }
        for (FFunction function : instanceFunctions.values()) {
            sb.append(function.headerToString()).append(", ");
        }
        return sb.append("\n}");
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append("{\n");
        for (FField field : staticFields.values()) {
            field.toString(sb).append('\n');
        }
        for (FField field : instanceFields.values()) {
            field.toString(sb).append('\n');
        }
        for (FFunction function : staticFunctions.values()) {
            function.toString(sb).append('\n');
        }
        for (FFunction function : instanceFunctions.values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }

    @Override
    public String toString() {
        return tS();
    }
}
