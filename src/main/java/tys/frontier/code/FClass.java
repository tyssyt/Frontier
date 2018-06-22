package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.Multimap;
import com.google.common.primitives.Booleans;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.literal.FNull;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.DisjunctUnionSetView;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;
import tys.frontier.util.Utils;

import java.util.*;

public class FClass implements IdentifierNameable, HasVisibility, StringBuilderToString {

    private FClassIdentifier identifier;
    private FVisibilityModifier visibility;

    private Set<FClass> superClasses = new LinkedHashSet<>();
    private Set<FClass> subClasses = new HashSet<>();

    private FVisibilityModifier constructorVisibility;

    private FLocalVariable thiz;

    protected BiMap<FVariableIdentifier, FField> fields = HashBiMap.create();
    protected Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    private FBlock instanceInitializer;
    private FBlock staticInitializer;

    public FClass (FClassIdentifier identifier, FVisibilityModifier visibility) {
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

    public void generateConstructor() {
        FVisibilityModifier visibility = constructorVisibility == null ? FVisibilityModifier.PRIVATE : constructorVisibility;
        try {
            addFunction(FConstructor.create(visibility, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }

    public boolean addSuperClass(FClass parentClass) {
        invalidateCachedChildData();
        parentClass.subClasses.add(this);
        return superClasses.add(parentClass);
    }

    private void invalidateCachedChildData() {
        cachedInheritedTypes = null;
        cachedAllFields = null;
    }

    public Set<FClass> getSuperClasses() {
        return superClasses;
    }

    public Set<FClass> getSubClasses() {
        return subClasses;
    }

    public boolean isSubType(FClass other) {
        if (superClasses.contains(other))
            return true;
        for (FClass parentClass : superClasses) {
            if (parentClass.isSubType(other))
                return true;
        }
        return false;
    }

    public boolean isSuperType(FClass other) {
        return other.isSubType(this);
    }

    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    public BiMap<FVariableIdentifier, FField> getFields() {
        return fields;
    }

    public Multimap<FFunctionIdentifier, FFunction> getFunctions() {
        return functions;
    }
    @Override
    public FClassIdentifier getIdentifier () {
        return identifier;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FLocalVariable getThis() {
        return thiz;
    }

    public FExpression getDefaultValue() {
        return new FLiteralExpression(FNull.INSTANCE);
    }

    public Collection<FFunction> getFunctions (FFunctionIdentifier identifier) {
        return functions.get(identifier);
    }

    public FField resolveField(FVariableIdentifier identifier) throws FieldNotFound {
        FField field = fields.get(identifier);
        if (field != null)
            return field;
        for (FClass parentClass : superClasses) {
            try {
                return parentClass.resolveField(identifier);
            } catch (FieldNotFound ignored) {}
        }
        throw new FieldNotFound(identifier);
    }

    /**
     * Resolves a function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * TODO once we have class hiereachies, also check parents
     * @param identifier the identifier of the function to resolve
     * @param paramTypes the parameter Types of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveFunction (FFunctionIdentifier identifier, List<FClass> paramTypes) throws FunctionNotFound {
        int bestCost = Integer.MAX_VALUE;
        Pair<FFunction, boolean[]> res = new Pair<>();
        for (FFunction f : functions.get(identifier)) {
            try {
                boolean[] cost = f.castSignatureFrom(paramTypes);
                int costSum = Booleans.countTrue(cost);
                if (costSum < bestCost) {
                    bestCost = costSum;
                    res.a = f;
                    res.b = cost;
                    if (bestCost == 0)
                        return res;
                } else if (costSum == bestCost) {
                    res.a = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
                }
            } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
        }

        for (FClass parentClass : superClasses) {
            try {
                Pair<FFunction, boolean[]> parentRes = parentClass.resolveFunction(identifier, paramTypes);
                if (Booleans.countTrue(parentRes.b) == 0)
                    return parentRes;
                if (res.a == null)
                    res = parentRes;
            } catch (FunctionNotFound ignored) {}
        }

        if (res.a == null)
            throw new FunctionNotFound(identifier, paramTypes);
        return res;
    }

    public FBlock getInstanceInitializer() {
        if (instanceInitializer == null) {
            //TODO order fields that depend on others so they can be initialzed in a nice order, error on cyclic dependencies
            List<FStatement> statements = new ArrayList<>();
            for (FField field : fields.values())
                if (!field.isStatic())
                    field.getAssignment().ifPresent(statements::add);
            instanceInitializer = FBlock.from(statements);
        }
        return instanceInitializer;
    }

    public FBlock getStaticInitializer() {
        if (staticInitializer == null) {
            //TODO order fields that depend on others so they can be initialzed in a nice order, error on cyclic dependencies
            List<FStatement> statements = new ArrayList<>();
            for (FField field : fields.values())
                if (field.isStatic())
                    field.getAssignment().ifPresent(statements::add);
            staticInitializer = FBlock.from(statements);
        }
        return staticInitializer;
    }

    public void addField (FField field) throws IdentifierCollision {
        FField old = fields.put(field.getIdentifier(), field);
        if (old != null) {
            throw new IdentifierCollision(field, old);
        }
        if (field.isStatic())
            staticInitializer = null;
        else
            instanceInitializer = null;
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        checkFunctionCollision(function);
        functions.put(function.getIdentifier(), function);
    }

    private void checkFunctionCollision (FFunction function) throws SignatureCollision {
        for (FFunction other : getFunctions(function.getIdentifier())) {
            if (function.getSignature().collidesWith(other.getSignature()))
                throw new SignatureCollision(function, other, this);
        }
    }

    private Set<FClass> cachedInheritedTypes;
    public Set<FClass> getAllInheritedTypes() {
        if (cachedInheritedTypes == null) {
            cachedInheritedTypes = new HashSet<>();
            cachedInheritedTypes.add(this);
            for (FClass parentClass : superClasses) {
                cachedInheritedTypes.addAll(parentClass.getAllInheritedTypes());
            }
        }
        return cachedInheritedTypes;
    }

    private Set<FField> cachedAllFields;
    public Set<FField> getAllFields() {
        if (cachedAllFields == null) {
            Collection<Set<FField>> fieldSets = new ArrayList<>();
            for (FClass fClass : getAllInheritedTypes()) {
                fieldSets.add(fClass.getFields().values());
            }
            cachedAllFields = DisjunctUnionSetView.of(fieldSets);
        }
        return cachedAllFields;
    }

    public <C,Fi,Fu,S,E> C accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterClass(this);
        List<Fi> fields = new ArrayList<>(this.fields.size());
        for (FField f : this.fields.values()) {
            visitor.enterField(f);
            fields.add(visitor.exitField(f, f.getAssignment().map(assignment -> assignment.accept(visitor))));
        }
        List<Fu> functions = new ArrayList<>(this.functions.values().size());
        for (FFunction f : this.functions.values()) {
            visitor.enterFunction(f);
            functions.add(visitor.exitFunction(f, f.getBody().map(body -> body.accept(visitor))));
        }
        return visitor.exitClass(this, fields, functions);
    }

    public String headerToString() {
        return visibility + " class " + identifier;
    }

    public StringBuilder summary(StringBuilder sb) {
        sb.append(headerToString()).append("{\n  ");
        for (FField field : fields.values()) {
            field.toString(sb).append(", ");
        }
        sb.append("\n  ");
        for (FFunction function : functions.values()) {
            sb.append(function.headerToString()).append(", ");
        }
        return sb.append("\n}");
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append("{\n");
        for (FField field : fields.values()) {
            field.toString(sb).append('\n');
        }
        for (FFunction function : functions.values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }

    @Override
    public String toString() {
        return tS();
    }
}
