package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FParameter;
import tys.frontier.code.FType;
import tys.frontier.code.FTypeVariable;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.util.Utils;

import java.util.*;

public class InstantiableFunctionCopy extends ForwardingFunction {

    //TODO the places in code where this is used can be simplified where we just create a copy of the type of base function that uses instantiable vars instead

    private FType newReturnType;
    private ImmutableList<FParameter> newParams;
    private Signature newSignature;

    private Map<FTypeIdentifier, FTypeVariable> newParameters;
    private List<FTypeVariable> newParametersList;

    private InstantiableFunctionCopy(FFunction base) {
        super(base);
        assert !(base instanceof InstantiableFunctionCopy);

        boolean baseFinished = base.getBody().isPresent();
        //create a non fixed copy for each var
        Map<FTypeVariable, FTypeVariable> varMap = new LinkedHashMap<>(); //linked map so newParametersList retains base order
        for (FTypeVariable var : base.getParametersList()) {
            if (!var.isFixed() && !baseFinished)
                Utils.NYI("getting a function address with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing
            FTypeVariable copy = var.copy(false);
            varMap.put(var, copy);
        }

        //noinspection unchecked
        TypeInstantiation typeInstantiation = TypeInstantiation.create((Map)varMap);

        newReturnType = typeInstantiation.getType(base.getType());
        newParams = createParams(base.getParams(), typeInstantiation);
        //noinspection unchecked
        newParameters = (Map) Utils.asMap(varMap.values());
        newParametersList = new ArrayList<>(varMap.values());
        newSignature = new Signature(this);
    }

    private static ImmutableList<FParameter> createParams(ImmutableList<FParameter> original, TypeInstantiation typeInstantiation) {
        ImmutableList.Builder<FParameter> res = ImmutableList.builder();
        for (FParameter p : original) {
            res.add(FParameter.create(p.getIdentifier(), typeInstantiation.getType(p.getType()), p.hasDefaultValue()));
        }
        return res.build();
    }

    public static FFunction instantiableCopyOf(FFunction base) {
        if (base.getParameters().isEmpty())
            return base;
        return new InstantiableFunctionCopy(base);
    }

    @Override
    public ImmutableList<FParameter> getParams() {
        return newParams;
    }

    @Override
    public boolean addCall(FFunctionCall call) {
        return Utils.cantHappen();
    }

    @Override
    public List<FFunctionCall> getCalledBy() {
        return Utils.cantHappen();
    }

    @Override
    public Optional<FBlock> getBody() {
        return Utils.cantHappen();
    }

    @Override
    public void setBody(FBlock body) {
        Utils.cantHappen();
    }

    @Override
    public FType getType() {
        return newReturnType;
    }

    @Override
    public Signature getSignature() {
        return newSignature;
    }

    @Override
    public Map<FTypeIdentifier, FTypeVariable> getParameters() {
        return newParameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return newParametersList;
    }

    @Override
    public boolean isInstantiation() {
        return true;
    }

    @Override
    public TypeInstantiation getTypeInstantiationToBase() {
        return Utils.cantHappen();
    }

    @Override
    public FFunction getBaseR() {
        return proxy.getBaseR();
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) { //TODO almost copy of FInst
        if (typeInstantiation.isEmpty())
            return this;
        Map<FTypeVariable, FType> baseMap = new HashMap<>(newParameters.size());
        for (FTypeVariable var : newParametersList) {
            FType type = typeInstantiation.getTypeMap().get(var);
            if (type == null) { //the only var tha is allowed not to be resolved is the return type
                assert newReturnType == var;
                type = var;
            }
            baseMap.put(proxy.getParameters().get(var.getIdentifier()), type);
        }
        return proxy.getInstantiation(TypeInstantiation.create(baseMap));
    }
}