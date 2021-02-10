package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.CArrayIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.List;

public class CArray extends FInstantiatedClass {

    //TODO once we have fancy iterators, I want to be able to iterate over a cArray, where I obv need to specify the size before doing so

    public static final FIdentifier OF = new FIdentifier("of");
    public static final FIdentifier COPY_TO_F_ARRAY = new FIdentifier("copyToFArray");

    private CArray(FIdentifier newIdentifier, FClass baseClass, ImmutableList<FType> instantiatedParameters) {
        super(newIdentifier, baseClass, instantiatedParameters);
    }

    public static CArray getArrayFrom(FType baseType) {
        try {
            return (CArray) FArray.FBaseArray.C_INSTANCE.getInstantiation(ImmutableList.of(baseType));
        } catch (WrongNumberOfTypeArguments wrongNumberOfTypeArguments) {
            return Utils.cantHappen();
        }
    }

    public FType getBaseType() {
        return Iterables.getOnlyElement(getParametersList());
    }

    static class CBaseArray extends FPredefinedClass {

        CBaseArray(FArray.FBaseArray fArray) {
            super(new CArrayIdentifier(fArray.getBase().getIdentifier()));
            addDefaultFunctions();
            FTypeVariable baseType = fArray.getBase();
            setParameters(List.of(baseType), List.of(Variance.Invariant));

            DefaultNamespace namespace = getNamespace();
            //addDefaultFunctions(); TODO should only be added once for "base class"

            Pair<FFunction, FFunction> access = Access.createPredefined(this, baseType, FIntN._32, FIntN._32);
            namespace.addFunctionTrusted(access.a);
            namespace.addFunctionTrusted(access.b);

            FunctionBuilder builder = new FunctionBuilder().setMemberOf(namespace).setPredefined(true);

            builder.setIdentifier(OF);
            namespace.addFunctionTrusted(builder.setParams(baseType).setReturnType(this).build());

            builder.setIdentifier(COPY_TO_F_ARRAY);
            namespace.addFunctionTrusted(builder.setParams(this, FIntN._32).setReturnType(fArray).build());

            setConstructorVisibility(FVisibilityModifier.PRIVATE);
            namespace.addFunctionTrusted(FConstructor.create(FVisibilityModifier.PRIVATE, this, true));
            fArray.init(this);
        }

        @Override
        protected FInstantiatedClass createInstantiatedClass(ImmutableList<FType> args) {
            return new CArray(new CArrayIdentifier(Iterables.getOnlyElement(args).getIdentifier()), this, args);
        }
    }

}
