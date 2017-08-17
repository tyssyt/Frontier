package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import tys.frontier.code.identifier.FClassIdentifier;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.type.FClassType;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.Typed;
import tys.frontier.parser.syntaxTree.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxTree.syntaxErrors.SignatureCollision;

import java.util.LinkedHashMap;
import java.util.Map;

public class FClass implements IdentifierNameable, Typed {

    private FClassIdentifier identifier;
    private FVisibilityModifier visibility;

    //type implicitly defined by this class
    private FClassType type;
    private FVariable thiz;

    private Map<FVariableIdentifier, FField> fields = new LinkedHashMap<>();
    private Multimap<FFunctionIdentifier, FFunction> functions = ArrayListMultimap.create();

    public FClass(FClassIdentifier identifier, FVisibilityModifier visibility) {
        this.identifier = identifier;
        this.visibility = visibility;
        this.type = new FClassType(this);
    }

    public void addField (FField field) throws IdentifierCollision {
        FField old = fields.put(field.getIdentifier(), field);
        if (old != null) {
            throw new IdentifierCollision(field, old, this);
        }
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        for (FFunction other : functions.get(function.getIdentifier())) {
            if (function.hasSameSignatureAs(other))
                throw new SignatureCollision(function, other, this);
        }
        functions.put(function.getIdentifier(), function);
    }

    @Override
    public FClassIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FType getType() {
        return type;
    }

    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FVariable getThis() {
        if (thiz == null) {
            thiz = new FVariable(FVariableIdentifier.THIS, this.type);
        }
        return thiz;
    }

    @Override
    public String toString() {
        return visibility + " class " + identifier + "{\n  " + fields.values() + "\n  " + functions.values() + "\n}";
    }
}
