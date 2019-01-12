package tys.frontier.code;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.Utils;

import java.util.*;

public class FClass extends FType implements HasVisibility, HasTypeParameters<FClass> {

    protected FVisibilityModifier visibility;
    private FVisibilityModifier constructorVisibility;
    private Map<FTypeIdentifier, FTypeVariable> parameters;
    private List<FTypeVariable> parametersList;
    private Map<TypeInstantiation, FInstantiatedClass> instantiations;


    private Map<FType, FField> delegates = new HashMap<>();

    protected Map<FFunction, String> uniqueFunctionNames;

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility) {
        this(identifier, visibility, Collections.emptyMap());
    }

    public FClass(FTypeIdentifier identifier, FVisibilityModifier visibility, Map<FTypeIdentifier, FTypeVariable> parameters) {
        super(identifier);
        this.visibility = visibility;
        if (parameters.isEmpty()) {
            this.parameters = Collections.emptyMap();
            this.parametersList = Collections.emptyList();
            this.instantiations = Collections.emptyMap();
        } else {
            this.parameters = parameters;
            this.parametersList = new ArrayList<>(parameters.values());
            this.instantiations = new MapMaker().concurrencyLevel(1).weakValues().makeMap();
        }
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
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    public FVisibilityModifier getConstructorVisibility() {
        return constructorVisibility;
    }

    public void setConstructorVisibility(FVisibilityModifier constructorVisibility) {
        this.constructorVisibility = constructorVisibility;
    }

    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return parameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return parametersList;
    }

    @Override
    public FClass getInstantiation(TypeInstantiation typeInstantiation) {
        TypeInstantiation intersected = typeInstantiation.intersect(parametersList);
        if (intersected.isEmpty())
            return this;
        return instantiations.computeIfAbsent(typeInstantiation, i -> new FInstantiatedClass(this, intersected));
    }

    public void addDelegate(FField field) {
        assert field.getMemberOf() == this;
        assert field.getType() instanceof FClass;
        if (field.getVisibility() != FVisibilityModifier.PRIVATE)
            delegates.put(field.getType(), field);
    }

    public List<FField> getDelegate(FType toType) {
        List<FField> res = new ArrayList<>();
        if (getDelegate(toType, res))
            return res;
        else
            return null;
    }

    private boolean getDelegate(FType toType, List<FField> res) {
        FField f = delegates.get(toType);
        if (f != null) {
            res.add(f);
            return true;
        }

        for (Map.Entry<FType, FField> entry : delegates.entrySet()) {
            res.add(entry.getValue());
            if (entry.getKey() instanceof FClass && ((FClass) entry.getKey()).getDelegate(toType, res))
                return true;
            res.remove(res.size() - 1);
        }
        return false;
    }

    public void addField (FField field) throws IdentifierCollision {
        if (field.isInstance()) {
            FField old = getInstanceFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        } else {
            FField old = getStaticFields().put(field.getIdentifier(), field);
            if (old != null) {
                throw new IdentifierCollision(field, old);
            }
        }
    }

    public void addFunction (FFunction function) throws SignatureCollision {
        for (FFunction other : getFunctions().get(function.getIdentifier())) {
            if (function.getSignature().collidesWith(other.getSignature()))
                throw new SignatureCollision(function, other);
        }
        getFunctions().put(function.getIdentifier(), function);
        uniqueFunctionNames = null;
    }

    public FConstructor getConstructor() {
        return (FConstructor) Iterables.getOnlyElement(getFunctions().get(FConstructor.IDENTIFIER));
    }

    public FConstructor generateConstructor() {
        FVisibilityModifier visibility = constructorVisibility == null ? FVisibilityModifier.PRIVATE : constructorVisibility;
        try {
            addFunction(FConstructor.createMalloc(this));
            FConstructor res = FConstructor.create(visibility, this);
            addFunction(res);
            return res;
        } catch (SignatureCollision signatureCollision) {
            return Utils.handleException(signatureCollision);
        }
    }

    public Map<FFunction, String> getUniqueFunctionNames() {
        if (uniqueFunctionNames == null) {
            uniqueFunctionNames = computeUniqueFunctionNames();
        }
        return uniqueFunctionNames;
    }

    private Map<FFunction, String> computeUniqueFunctionNames() {
        Map<FFunction, String> res = new HashMap<>();
        ArrayListMultimap<FFunctionIdentifier, FFunction> allFuncs = ArrayListMultimap.create(getFunctions());
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

    public void removeUnreachable(Reachability.ReachableClass reachable) {
        getStaticFields().values().retainAll(reachable.reachableFields);
        getInstanceFields().values().retainAll(reachable.reachableFields);
        getFunctions().values().retainAll(reachable.reachableFunctions);
    }

    public <C,Fi,Fu,S,E> C accept(ClassVisitor<C,Fi,Fu,S,E> visitor) {
        visitor.enterType(this);
        List<Fi> fields = new ArrayList<>(this.getInstanceFields().size() + this.getStaticFields().size());
        for (FField f : this.getInstanceFields().values()) {
            fields.add(f.accept(visitor));
        }
        for (FField f : this.getStaticFields().values()) {
            fields.add(f.accept(visitor));
        }
        List<Fu> functions = new ArrayList<>(this.getFunctions().size());
        for (FFunction f : this.getFunctions().values()) {
            functions.add(f.accept(visitor));
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
        for (FFunction function : getFunctions().values()) {
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
        for (FFunction function : getFunctions().values()) {
            function.toString(sb).append('\n');
        }
        return sb.append("\n}");
    }

}
