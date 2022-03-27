package tys.frontier.code.namespace;

import com.google.common.collect.*;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.StaticField;
import tys.frontier.code.expression.*;
import tys.frontier.code.function.*;
import tys.frontier.code.functionResolve.FunctionResolver;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FExpressionStatement;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.*;

public class DefaultNamespace implements Namespace {

    private FIdentifier identifier;
    private FVisibilityModifier visibility;
    private FClass fClass; //optional
    private Location location;
    private NativeDecl nativeDecl;

    private BiMap<FIdentifier, StaticField> staticFields = HashBiMap.create();
    private ListMultimap<FIdentifier, Signature> lhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private ListMultimap<FIdentifier, Signature> rhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();

    private NameGenerator lambdaNames = new NameGenerator("λ", "");
    private NameGenerator returnTypeNames;

    public DefaultNamespace(Location location, FIdentifier identifier, FVisibilityModifier visibility, NativeDecl nativeDecl, FClass fClass) {
        this.location = location;
        this.identifier = identifier;
        this.visibility = visibility;
        this.nativeDecl = nativeDecl;
        this.returnTypeNames = new NameGenerator("?" + getIdentifier().name + "ret.", "");
        this.fClass = fClass;
    }

    @Override
    public FIdentifier getIdentifier() {
        return identifier;
    }

    @Override
    public FIdentifier nextReturnTypeIdentifier() {
        return new FIdentifier(returnTypeNames.next());
    }

    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public FClass getType() {
        return fClass;
    }

    @Override
    public Location getLocation() {
        return location;
    }

    public boolean isEmpty() {
        return rhsFunctions.isEmpty();
    }

    public NativeDecl getNative() {
        return nativeDecl;
    }

    public void addField(StaticField field) throws IdentifierCollision, SignatureCollision {
        FField old = getStaticFields().put(field.getIdentifier(), field);
        if (old != null) {
            throw new IdentifierCollision(field, old);
        }
        addFunction(field.getGetter());
        addFunction(field.getSetter());

        if (field.getType() instanceof FFunctionType) { //TODO similar code in FClass
            FFunctionType functionType = (FFunctionType) field.getType();
            List<FType> params = FTuple.unpackType(functionType.getIn());
            FBaseFunction dynamicCall = new FunctionBuilder(field.getIdentifier(), this)
                    .setParams(params).setReturnType(functionType.getOut())
                    .setLocation(field.getLocation()).setVisibility(field.getVisibility()).build();

            //TODO @PositionForGeneratedCode
            FFunctionCall fieldGet = FFunctionCall.create(null, field.getGetter().getSignature(), List.of());
            List<FExpression> paramExprs = Utils.map(dynamicCall.getSignature().getParameters(), p -> new FVariableExpression(null, p));
            DynamicFunctionCall call = DynamicFunctionCall.createTrusted(null, fieldGet, paramExprs);
            FStatement body = functionType.getOut() == FTuple.VOID ?
                    new FExpressionStatement(null, call) :
                    FReturn.createTrusted(null, call, dynamicCall);
            dynamicCall.setBody(FBlock.from(null, body));
            addFunction(dynamicCall);
        }
    }

    public void addFieldTrusted(StaticField field) {
        try {
            addField(field);
        } catch (IdentifierCollision | SignatureCollision e) {
            Utils.cantHappen();
        }
    }

    public BiMap<FIdentifier, StaticField> getStaticFields() {
        return staticFields;
    }

    public ListMultimap<FIdentifier, Signature> getFunctions(boolean lhsSignatures) {
        return lhsSignatures ? lhsFunctions : rhsFunctions;
    }

    public void addFunction(FFunction function) throws SignatureCollision {
        addFunction(function.getSignature(), false);
        if (function.getLhsSignature() != null)
            addFunction(function.getLhsSignature(), true);
    }

    private void addFunction(Signature signature, boolean lhs) throws SignatureCollision {
        List<Signature> others = getFunctions(lhs).get(signature.getFunction().getIdentifier());
        if (others.size() > 0) {
            if (signature.getFunction().getParameters().size() > 0 || others.get(0).getFunction().getParameters().size() > 0)
                throw new SignatureCollision(signature, others.get(0)); // cannot overload generic functions
        }
        for (Signature other : others) {
            if (SignatureCollision.collide(signature, other))
                throw new SignatureCollision(signature, other);
        }
        getFunctions(lhs).put(signature.getFunction().getIdentifier(), signature);
    }

    public void addFunctionTrusted(FFunction function) {
        try {
            addFunction(function);
        } catch (SignatureCollision signatureCollision) {
            Utils.cantHappen();
        }
    }

    @Override
    public FunctionResolver.Result resolveFunction(FIdentifier identifier, List<FType> positionalArgs, Map<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve, List<UnboundExpression> unbounds) throws FunctionNotFound {
        assert returnType == null || !lhsResolve;
        return FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, this, lhsResolve, unbounds);
    }

    public FIdentifier getFreshLambdaName() {
        return new FIdentifier(lambdaNames.next());
    }

    public void removeUnreachable(Reachability.ReachableNamespace reachable) {
        if (nativeDecl != null || (fClass != null && fClass.isPredefined()))
            return;

        getFunctions(false).values().removeIf(s -> !reachable.isReachable(s.getFunction()));
        getFunctions(true).clear(); //not needed after this point
        getStaticFields().values().removeIf(f -> !reachable.isReachable(f));

        if (fClass != null)
            fClass.getInstanceFields().values().removeIf(f -> !reachable.isReachable(f));
    }

    public Map<FFunction, String> computeUniqueFunctionNames() {
        Map<FFunction, String> res = new HashMap<>();
        for (List<Signature> list : Multimaps.asMap(rhsFunctions).values()) {
            String name = list.get(0).getFunction().getIdentifier().name;

            if (list.size() == 1) {
                res.put(list.get(0).getFunction(), name);
                continue;
            }

            //TODO when multithreading is used we might need to copy the list first before sorting to avoid race conditions while sorting
            Signature[] array = list.toArray(new Signature[0]);
            Arrays.sort(array, (s1, s2) -> {
                ImmutableList<FParameter> p1 = s1.getParameters();
                ImmutableList<FParameter> p2 = s2.getParameters();
                int c = p1.size() - p2.size();
                if (c != 0)
                    return c;
                for (int i=0; i<p1.size(); i++) {
                    String id1 = p1.get(i).getType().getIdentifier().name;
                    String id2 = p2.get(i).getType().getIdentifier().name;
                    c = id1.compareTo(id2);
                    if (c != 0)
                        return c;
                }
                return 0;
            });
            for (int i=0; i<array.length; i++) {
                res.put(array[i].getFunction(), name + "#" + i);
            }
        }
        return res;
    }

    public <N,C,Fi,Fu,S,E> N accept(ClassVisitor<N, C, Fi, Fu, S, E> visitor) {
        visitor.enterNamespace(this);
        C c = fClass != null ? fClass.accept(visitor) : null;
        List<Fi> staticFields = new ArrayList<>(this.getStaticFields().size());
        for (StaticField field : this.getStaticFields().values())
            staticFields.add(field.accept(visitor));
        List<Fu> functions = new ArrayList<>(this.getFunctions(false).size());
        for (Signature s : this.getFunctions(false).values())
            functions.add(s.getFunction().accept(visitor));
        return visitor.exitNamespace(this, Optional.ofNullable(c), staticFields, functions);
    }
}
