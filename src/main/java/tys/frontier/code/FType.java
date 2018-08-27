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
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.DisjunctUnionSetView;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.*;

public abstract class FType implements IdentifierNameable, HasVisibility, StringBuilderToString {
    protected FTypeIdentifier identifier;
    protected FVisibilityModifier visibility;
    protected Set<FInterface> extendedInterfaces = new LinkedHashSet<>();
    protected FLocalVariable thiz;

    protected BiMap<FVariableIdentifier, FField> instanceFields = HashBiMap.create();
    protected BiMap<FVariableIdentifier, FField> staticFields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> instanceFunctions = ArrayListMultimap.create();
    protected Multimap<FFunctionIdentifier, FFunction> staticFunctions = ArrayListMultimap.create();

    protected Set<FType> subTypes = new HashSet<>();

    public FType(FTypeIdentifier identifier, FVisibilityModifier visibility) {
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

    public boolean addSuperType(FType superType) throws SyntaxError {
        invalidateCachedParentData();
        superType.subTypes.add(this);
        if (superType instanceof FInterface)
            return extendedInterfaces.add(((FInterface) superType));
        else if (superType instanceof FClass)
            return addSuperClass(((FClass) superType));
        else
            return Utils.cantHappen();
    }

    protected abstract boolean addSuperClass(FClass superClass) throws SyntaxError;

    protected void invalidateCachedParentData() {
        cachedInheritedTypes = null;
    }

    public Set<FInterface> getExtendedInterfaces() {
        return extendedInterfaces;
    }

    public Set<FType> getSuperTypes() {
        return new HashSet<>(extendedInterfaces);
    }

    public Set<FType> getSubTypes() {
        return subTypes;
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

    @Override
    public FTypeIdentifier getIdentifier () {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public BiMap<FFunction.Signature, FFunction> getNonImplementedFunctions() { //TODO override in FClass
        BiMap<FFunction.Signature, FFunction> res = HashBiMap.create();
        for (FType superType : getSuperTypes())
            res.putAll(superType.getNonImplementedFunctions());
        for (FFunction function : instanceFunctions.values())
            if (function.isAbstract())
                res.put(function.getSignature(), function);
            else
                res.remove(function.getSignature());
        return res;
    }

    public FLocalVariable getThis() {
        return thiz;
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

    public Set<FField> getFields() {
        return DisjunctUnionSetView.of(instanceFields.values(), staticFields.values());
    }

    public Collection<FFunction> getInstanceFunctions (FFunctionIdentifier identifier) {
        return instanceFunctions.get(identifier);
    }

    public Collection<FFunction> getStaticFunctions (FFunctionIdentifier identifier) {
        return staticFunctions.get(identifier);
    }

    /**
     * Resolves an instance function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * @param identifier the identifier of the function to resolve
     * @param paramTypes the parameter Types of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveInstanceFunction (FFunctionIdentifier identifier, List<FType> paramTypes) throws FunctionNotFound {
        return new FunctionResolver(identifier, paramTypes).resolve();
    }

    /**
     * Resolves a static function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * @param identifier the identifier of the function to resolve
     * @param paramTypes the parameter Types of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveStaticFunction (FFunctionIdentifier identifier, List<FType> paramTypes) throws FunctionNotFound {
        return new FunctionResolver(identifier, paramTypes).resolveStatic();
    }

    private class FunctionResolver {
        private int bestCost = Integer.MAX_VALUE;
        private FFunction bestFunction;
        private boolean[] bestCostArray;

        private FFunctionIdentifier identifier;
        private List<FType> paramTypes;

        FunctionResolver(FFunctionIdentifier identifier, List<FType> paramTypes) {
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

            for (FType superType : getSuperTypes()) {
                try {
                    Pair<FFunction, boolean[]> parentRes = superType.resolveInstanceFunction(identifier, paramTypes);
                    if (updateCost(parentRes.b, parentRes.a))
                        return result();
                } catch (FunctionNotFound ignored) {}
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
                if (bestFunction.getOverwrittenBy().contains(newFunction))
                    bestFunction = newFunction;
                else if (!bestFunction.getOverrides().contains(newFunction))
                    bestFunction = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
            }
            return false;
        }
    }

    public void addField (FField field) throws IdentifierCollision, InterfaceInstanceField {
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
    }

    private Set<FType> cachedInheritedTypes;
    public Set<FType> getAllInheritedTypes() {
        if (cachedInheritedTypes == null) {
            cachedInheritedTypes = new HashSet<>();
            cachedInheritedTypes.add(this);
            for (FType superType : getSuperTypes()) {
                cachedInheritedTypes.addAll(superType.getAllInheritedTypes());
            }
        }
        return cachedInheritedTypes;
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

    public abstract String headerToString();

    public StringBuilder summary(StringBuilder sb) {
        sb.append(headerToString()).append("{\n  ");
        for (FField field : staticFields.values()) {
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
