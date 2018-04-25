package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;
import com.google.common.primitives.Booleans;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.Pair;
import tys.frontier.util.StringBuilderToString;

import java.util.*;

public class FClass implements IdentifierNameable, StringBuilderToString {

    private FClassIdentifier identifier;
    private FVisibilityModifier visibility;

    private FLocalVariable thiz;

    protected Map<FVariableIdentifier, FField> fields = new LinkedHashMap<>();
    protected Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    private FBlock instanceInitializer;
    private FBlock staticInitializer;

    public FClass (FClassIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
        thiz = new FLocalVariable(FVariableIdentifier.THIS, this);
        addDefaultFunctions();
    }

    protected FClass(FClassIdentifier identifier, FVisibilityModifier visibility, boolean b) {
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
            throw new RuntimeException(e);
        }
    }

    public Map<FVariableIdentifier, FField> getFields() {
        return fields;
    }

    public Multimap<FFunctionIdentifier, FFunction> getFunctions() {
        return functions;
    }
    @Override
    public FClassIdentifier getIdentifier () {
        return identifier;
    }

    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FLocalVariable getThis () {
        return thiz;
    }

    public FField getField (FVariableIdentifier identifier) {
        return fields.get(identifier);
    }

    public Collection<FFunction> getFunctions (FFunctionIdentifier identifier) {
        return functions.get(identifier);
    }

    public FFunction getFunction (FFunction.Signature signature) {
        for (FFunction f : functions.get(signature.getIdentifier()))
            if (f.getSignature().equals(signature))
                return f;
        return null;
    }

    /**
     * Resolves a function call.
     * Will potentially implicitly cast the parameters to find a fitting function
     * TODO once we have class hiereachies, also check parents
     * @param signature the signature of the function to resolve
     * @return the resolved function, and a boolean array where the i-th value is true if the i-th parameter must be cast
     */
    public Pair<FFunction, boolean[]> resolveFunction (FFunction.Signature signature) throws FunctionNotFound {
        int bestCost = Integer.MAX_VALUE;
        Pair<FFunction, boolean[]> res = new Pair<>();
        for (FFunction f : functions.get(signature.getIdentifier())) {
            try {
                boolean[] cost = f.getSignature().castSignatureFrom(signature);
                int costSum = Booleans.countTrue(cost);
                if (costSum < bestCost) {
                    bestCost = costSum;
                    res.a = f;
                    res.b = cost;
                    if (bestCost == 0)
                        break;
                } else if (costSum == bestCost) {
                    res.a = null; //not obvious which function to call %TODO a far more descriptive error message then FNF
                }
            } catch (FFunction.IncompatibleSignatures | IncompatibleTypes ignored) {}
        }
        if (res.a == null)
            throw new FunctionNotFound(signature);
        return res;
    }

    public FBlock getInstanceInitializer() {
        if (instanceInitializer == null) {
            //TODO order fields that depend on others so they can be initialzed in a nice order, error on cyclic dependencies
            ImmutableList.Builder<FStatement> statements = ImmutableList.builder();
            for (FField field : fields.values())
                if (!field.isStatic())
                    field.getAssignment().ifPresent(statements::add);
            instanceInitializer = new FBlock(statements.build());
        }
        return instanceInitializer;
    }

    public FBlock getStaticInitializer() {
        if (staticInitializer == null) {
            //TODO order fields that depend on others so they can be initialzed in a nice order, error on cyclic dependencies
            ImmutableList.Builder<FStatement> statements = ImmutableList.builder();
            for (FField field : fields.values())
                if (field.isStatic())
                    field.getAssignment().ifPresent(statements::add);
            staticInitializer = new FBlock(statements.build());
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
        FFunction old = getFunction(function.getSignature());
        if (old != null)
            throw new SignatureCollision(function, old, this);
        functions.put(function.getIdentifier(), function);
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
            List<S> body = new ArrayList<>(f.getBody().size());
            for (FStatement s : f.getBody())
                body.add(s.accept(visitor));
            functions.add(visitor.exitFunction(f, body));
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
