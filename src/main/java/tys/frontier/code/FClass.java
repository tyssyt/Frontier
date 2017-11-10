package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.Operator.FEquals;
import tys.frontier.code.Operator.FHashCode;
import tys.frontier.code.Operator.FNotEquals;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.predefinedClasses.FPredefinedClass;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.util.StringBuilderToString;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class FClass implements IdentifierNameable, StringBuilderToString {

    static {
        FPredefinedClass.load();
    }

    private FClassIdentifier identifier;
    private FVisibilityModifier visibility;

    private FLocalVariable thiz;

    protected Map<FVariableIdentifier, FField> fields = new LinkedHashMap<>();
    protected Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    public FClass (FClassIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
        addDefaultFunctions();
    }

    protected FClass(FClassIdentifier identifier, FVisibilityModifier visibility, boolean b) {
        this.identifier = identifier;
        this.visibility = visibility;
    }

    protected void addDefaultFunctions() {
        FEquals eq = new FEquals(this);
        addFunctionInternal(eq);
        addFunctionInternal(new FNotEquals(eq));
        addFunctionInternal(new FHashCode(this));
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
        if (thiz == null) {
            thiz = new FLocalVariable(FVariableIdentifier.THIS, this);
        }
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

    public void addField (FField field) throws IdentifierCollision {
        FField old = fields.put(field.getIdentifier(), field);
        if (old != null) {
            throw new IdentifierCollision(field, old);
        }
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        FFunction old = getFunction(function.getSignature());
        if (old != null)
            throw new SignatureCollision(function, old, this);
        addFunctionInternal(function);
    }

    protected void addFunctionInternal (FFunction function) {
        functions.put(function.getIdentifier(), function);
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
}
