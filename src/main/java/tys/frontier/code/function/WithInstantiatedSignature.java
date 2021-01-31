package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

public class WithInstantiatedSignature extends ForwardingFunction {

    private TypeInstantiation instantiation;

    private Signature newSignature;
    private Signature newLhsSignature;

    public WithInstantiatedSignature(FFunction proxy, TypeInstantiation instantiation) {
        super(proxy);
        this.instantiation = instantiation;

        Signature sig = proxy.getLhsSignature() == null ? proxy.getSignature() : proxy.getLhsSignature();
        ImmutableList<FParameter> parameters = Utils.mapImmutable(sig.getParameters(), this::createParam);
        ImmutableList<FParameter> assignees = sig.getAssignees() == null ? null :
                Utils.mapImmutable(sig.getAssignees(), this::createParam);

        Pair<Signature, Signature> pair = Signature.createSignatures(this, parameters, assignees, instantiation.getType(proxy.getType()));
        newSignature = pair.a;
        newLhsSignature = pair.b;
    }

    private FParameter createParam(FParameter old) {
        return FParameter.create(old.getPosition(), old.getIdentifier(), instantiation.getType(old.getType()), old.hasDefaultValue());
    }

    @Override
    public boolean isInstantiation() {
        return true;
    }

    @Override
    public Signature getSignature() {
        return newSignature;
    }

    @Override
    public Signature getLhsSignature() {
        return newLhsSignature;
    }

    public TypeInstantiation getTypeInstantiation() {
        return instantiation;
    }

    @Override
    public TypeInstantiation getTypeInstantiationToBase() {
        return proxy.getTypeInstantiationToBase().then(instantiation);
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) {
        //TODO
        return Utils.NYI("");
    }
}
