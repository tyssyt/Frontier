package tys.frontier.code.predefinedClasses;

import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FOptionalIdentifier;
import tys.frontier.code.namespace.OptionalNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    private FType baseType;

    private FOptional(FType baseType) {
        super(new FOptionalIdentifier(baseType.getIdentifier()));
        assert !(baseType instanceof FOptional);
        assert baseType != FTuple.VOID;
        this.baseType = baseType;
        OptionalNamespace namespace = new OptionalNamespace(this);
        setNamespace(namespace);
        //addDefaultFunctions(); TODO should only be added once for "base class"
        namespace.addFunctionTrusted(new FunctionBuilder(UnaryOperator.NOT.identifier, namespace)
                .setVisibility(getVisibility()).setPredefined(true).setParams(this).setReturnType(FBool.INSTANCE).build());
    }

    public FType getBaseType() {
        return baseType;
    }

    @Override
    public OptionalNamespace getNamespace() {
        return (OptionalNamespace) super.getNamespace();
    }

    @Override
    public int concreteness() { //TODO once optionals use generics this is no longer necessary
        int res = baseType.concreteness();
        if (res == Integer.MAX_VALUE) //avoid overflow
            return Integer.MAX_VALUE;
        return res+1;
    }

    @Override
    public FVisibilityModifier getVisibility() {
        if (baseType instanceof FClass)
            return ((FClass) baseType).getVisibility();
        return super.getVisibility();
    }

    @Override
    public boolean canImplicitlyCast() {  //TODO once optionals use generics this is no longer necessary
        return true;
    }

    public static boolean canBeTreatedAsOptional(FType fType) {
        return fType instanceof FOptional ||
                (fType instanceof FTuple && Iterables.all(((FTuple) fType).getTypes(), FOptional::canBeTreatedAsOptional));
    }

    public static FBaseClass from(FType baseClass) {
        if (baseClass instanceof FTuple) {
            if (baseClass == FTuple.VOID)
                return FTuple.VOID;
            //the optional of a Tuple is a Tuple of Optionals
            FTuple tuple = (FTuple) baseClass;
            List<FType> optionalBases = new ArrayList<>(tuple.arity());
            for (FType base : tuple.getTypes()) {
                optionalBases.add(fromFlatten(base));
            }
            return (FTuple) FTuple.from(optionalBases);
        }
        return existing.computeIfAbsent(baseClass, FOptional::new);
    }

    public static FBaseClass fromFlatten(FType baseClass) {
        if (baseClass instanceof FOptional)
            return (FOptional) baseClass;
        else
            return from(baseClass);
    }
}
