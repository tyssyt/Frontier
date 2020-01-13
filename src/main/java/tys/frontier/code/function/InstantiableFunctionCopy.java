package tys.frontier.code.function;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.util.Utils;

import java.util.*;

public class InstantiableFunctionCopy extends WithInstantiatedSignature {

    //TODO the places in code where this is used can be simplified where we just create a copy of the type of base function that uses instantiable vars instead

    private Map<FIdentifier, FTypeVariable> newParameters;
    private List<FTypeVariable> newParametersList;

    private InstantiableFunctionCopy(FFunction base) {
        super(base, withNonFixedParameters(base));
        assert !(base instanceof InstantiableFunctionCopy);

        @SuppressWarnings({"rawtypes", "unchecked"})
        Map<FTypeVariable, FTypeVariable> varMap = (Map) getTypeInstantiation().getTypeMap();
        newParameters = Utils.asTypeMap(varMap.values());
        newParametersList = new ArrayList<>(varMap.values());
    }

    private static TypeInstantiation withNonFixedParameters(FFunction base) {
        boolean baseFinished = base.getBody().isPresent();
        //create a non fixed copy for each var
        Map<FTypeVariable, FTypeVariable> varMap = new LinkedHashMap<>(); //linked map so newParametersList retains base order
        for (FTypeVariable var : base.getParametersList()) {
            if (!var.isFixed() && !baseFinished)
                Utils.NYI("getting a function address with non fixed Parameters where the body is not finished"); //TODO for non recursive cases, this could be solved by waiting on f to finish parsing
            FTypeVariable copy = var.copy(false);
            varMap.put(var, copy);
        }

        //noinspection unchecked, rawtypes
        return TypeInstantiation.create((Map)varMap);
    }

    public static FFunction instantiableCopyOf(FFunction base) {
        if (base.getParameters().isEmpty())
            return base;
        return new InstantiableFunctionCopy(base);
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
    public Map<FIdentifier, FTypeVariable> getParameters() {
        return newParameters;
    }

    @Override
    public List<FTypeVariable> getParametersList() {
        return newParametersList;
    }

    @Override
    public FFunction getInstantiation(TypeInstantiation typeInstantiation) { //TODO this gets used kind of weird with the assert isEmpty and all the getInstantiation can be refactored to be used better...
        //InstantiableFunctionCopy is only used with addresses, so when this is called typeInstantiation should be emtpy, and all newParameters should be instantiated
        assert typeInstantiation.isEmpty();
        Map<FTypeVariable, FType> baseMap = new HashMap<>(newParameters.size());
        for (FTypeVariable var : newParametersList) {
            assert var.isResolved();
            baseMap.put(proxy.getParameters().get(var.getIdentifier()), var.getResolved());
        }
        return proxy.getInstantiation(TypeInstantiation.create(baseMap));
    }
}
