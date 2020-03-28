package tys.frontier.code.namespace;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.MultimapBuilder;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.HasVisibility;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.Signature;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FunctionResolver;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.syntaxErrors.FunctionNotFound;
import tys.frontier.parser.syntaxErrors.InvalidOpenDeclaration;
import tys.frontier.parser.syntaxErrors.SignatureCollision;
import tys.frontier.passes.analysis.reachability.Reachability;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.Utils;

import java.util.*;

public class DefaultNamespace implements Namespace, HasVisibility {

    private FIdentifier identifier;
    private FVisibilityModifier visibility;
    private boolean _native;
    private FClass fClass; //optional

    private ListMultimap<FIdentifier, Signature> lhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private ListMultimap<FIdentifier, Signature> rhsFunctions = MultimapBuilder.hashKeys().arrayListValues().build();
    private Map<FIdentifier, FFunction> openFunctions = new HashMap<>();
    private List<FFunction> remoteFunctions = new ArrayList<>();

    private NameGenerator lambdaNames = new NameGenerator("λ", "");
    private NameGenerator returnTypeNames;

    public DefaultNamespace(FIdentifier identifier, FVisibilityModifier visibility, boolean _native) {
        this.identifier = identifier;
        this.visibility = visibility;
        this._native = _native;
        this.returnTypeNames = new NameGenerator("?" + getIdentifier().name + "ret.", "");
    }

    public DefaultNamespace(FClass fClass) {
        this(fClass.getIdentifier(), fClass.getVisibility(), fClass.isNative() || fClass.isPredefined());
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

    @Override
    public FVisibilityModifier getVisibility() {
        return visibility;
    }

    @Override
    public FClass getType() {
        return fClass;
    }

    public boolean isEmpty() {
        return rhsFunctions.isEmpty();
    }

    public void setOpen(FFunction fFunction) throws InvalidOpenDeclaration {
        if (fClass != null && !fClass.getParametersList().isEmpty())
            throw new InvalidOpenDeclaration(fFunction, "in generic class");
        if (fFunction.getParameters().isEmpty())
            throw new InvalidOpenDeclaration(fFunction, "non generic function");
        FFunction old = openFunctions.put(fFunction.getIdentifier(), fFunction);
        assert old == null;
    }

    @Override
    public FFunction getOpen(FIdentifier identifier) {
        return openFunctions.get(identifier);
    }

    @Override
    public void addRemoteFunction(FFunction fFunction) {
        remoteFunctions.add(fFunction);
    }

    public List<FFunction> getRemoteFunctions() {
        return remoteFunctions;
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
        for (Signature other : getFunctions(lhs).get(signature.getFunction().getIdentifier())) {
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
    public FunctionResolver.Result softResolveFunction(FIdentifier identifier, List<FType> positionalArgs, ListMultimap<FIdentifier, FType> keywordArgs, FType returnType, boolean lhsResolve) throws FunctionNotFound {
        assert returnType == null || !lhsResolve;
        return FunctionResolver.resolve(identifier, positionalArgs, keywordArgs, returnType, this, lhsResolve);
    }

    public FIdentifier getFreshLambdaName() {
        return new FIdentifier(lambdaNames.next());
    }

    public void removeUnreachable(Reachability.ReachableNamespace reachable) {
        if (!_native) {
            getFunctions(false).values().removeIf(s -> !reachable.isReachable(s.getFunction()));
            getFunctions(true).clear(); //not needed after this point
        }

        if (fClass != null)
            fClass.removeUnreachable(reachable);
    }

    public <N,C,Fi,Fu,S,E> N accept(ClassVisitor<N, C, Fi, Fu, S, E> visitor) {
        visitor.enterNamespace(this);
        C c = fClass != null ? fClass.accept(visitor) : null;
        List<Fu> functions = new ArrayList<>(this.getFunctions(false).size());
        for (Signature s : this.getFunctions(false).values()) {
            functions.add(s.getFunction().accept(visitor));
        }
        return visitor.exitNamespace(this, Optional.ofNullable(c), functions);
    }
}
