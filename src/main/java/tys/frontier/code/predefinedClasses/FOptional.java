package tys.frontier.code.predefinedClasses;

import com.google.common.collect.Iterables;
import com.google.common.collect.MapMaker;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.OptionalNamespace;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FType;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

public class FOptional extends FPredefinedClass {

    //classes do not override equals, so we need to make sure we get the same object every time
    private static ConcurrentMap<FType, FOptional> existing = new MapMaker().concurrencyLevel(1).weakValues().makeMap();

    public static FIdentifier EXMARK = new FIdentifier("_!");

    private FType baseType;
    private FFunction exmark;

    private FOptional(FType baseType) {
        super(baseType.getIdentifier());
        assert !(baseType instanceof FOptional);
        assert baseType != FTuple.VOID;

        this.baseType = baseType;
        OptionalNamespace namespace = OptionalNamespace.create(this);
        setNamespace(namespace);
        //addDefaultFunctions(); TODO should only be added once for "base class"
        //TODO I have opt -> bool cast, why the hell do I need to define the not operator on optional?
        FunctionBuilder builder = new FunctionBuilder(UnaryOperator.NOT.identifier, namespace)
                .setVisibility(namespace.getVisibility()).setPredefined(true).setParams(this);
        namespace.addFunctionTrusted(builder.setReturnType(FBool.INSTANCE).build());
        exmark = builder.setIdentifier(EXMARK).setReturnType(baseType).build();
        namespace.addFunctionTrusted(exmark);
    }

    public FType getBaseType() {
        return baseType;
    }

    public FFunction getExmark() {
        return exmark;
    }

    @Override
    public OptionalNamespace getNamespace() {
        return (OptionalNamespace) super.getNamespace();
    }

    @Override
    public boolean canImplicitlyCast() {  //TODO once optionals use generics this is no longer necessary
        return true;
    }

    public static boolean canBeTreatedAsOptional(FType fType) {
        return fType instanceof FOptional ||
                (fType instanceof FTuple && Iterables.all(((FTuple) fType).getTypes(), FOptional::canBeTreatedAsOptional));
    }

    public static boolean isOriginalOptionalIdentifier(FIdentifier identifier) {
        return identifier.equals(UnaryOperator.NOT.identifier) || identifier.equals(EXMARK);
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
