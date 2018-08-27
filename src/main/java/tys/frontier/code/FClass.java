package tys.frontier.code;

import com.google.common.collect.Iterables;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.FVariableIdentifier;
import tys.frontier.parser.syntaxErrors.FieldNotFound;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.TwoSuperClasses;
import tys.frontier.util.DisjunctUnionSetView;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

public class FClass extends FType {

    private FClass superClass;

    private FVisibilityModifier constructorVisibility;

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        super(identifier, visibility);
    }

    @Override
    protected void invalidateCachedParentData() {
        super.invalidateCachedParentData();
        cachedAllFields = null;
    }

    @Override
    protected boolean addSuperClass(FClass superClass) throws SyntaxError {
        if (this.superClass != null)
            throw new TwoSuperClasses(this, this.superClass, superClass);
        this.superClass = superClass;
        return true;
    }

    public FClass getSuperClass() {
        return superClass;
    }

    @Override
    public Set<FType> getSuperTypes() {
        Set<FType> res = super.getSuperTypes();
        if (superClass != null) //FIXME remove check when our typeHierachy guarantees everyone has a superClass
            res.add(superClass);
        return res;
    }

    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(staticFunctions.get(FConstructor.IDENTIFIER));
    }

    public void removeConstructor() {
        staticFunctions.removeAll(FConstructor.IDENTIFIER);
    }

    public void generateConstructor() {
        FVisibilityModifier visibility = constructorVisibility == null ? FVisibilityModifier.PRIVATE : constructorVisibility;
        try {
            addFunction(FConstructor.create(visibility, this));
        } catch (SignatureCollision signatureCollision) {
            Utils.handleException(signatureCollision);
        }
    }

    public boolean isAbstract() {
        return !getNonImplementedFunctions().isEmpty();
    }

    public FField resolveField(FVariableIdentifier identifier) throws FieldNotFound {
        for (FClass cur = this; cur != null; cur = cur.getSuperClass()) {
            FField field = instanceFields.get(identifier);
            if (field != null)
                return field;
        }
        throw new FieldNotFound(identifier);
    }

    private Set<FField> cachedAllFields;
    public Set<FField> getAllFields() {
        if (cachedAllFields == null) {
            Collection<Set<FField>> fieldSets = new ArrayList<>();
            for (FClass cur = this; cur != null; cur = cur.getSuperClass()) {
                fieldSets.add(cur.getInstanceFields().values());
            }
            cachedAllFields = DisjunctUnionSetView.of(fieldSets);
        }
        return cachedAllFields;
    }

    @Override
    public String headerToString() {
        return visibility + " class " + identifier;
    }

    @Override
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
}
