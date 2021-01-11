package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FLiteralExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.identifier.FArrayIdentifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FIntNLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.loop.forImpl.ForByIdx;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FInstantiatedClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.parser.syntaxErrors.WrongNumberOfTypeArguments;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.List;
import java.util.Set;

import static java.util.Collections.emptySet;
import static tys.frontier.util.Utils.mutableSingletonList;

public class FArray extends FInstantiatedClass {

    public static final FIdentifier SIZE = new FIdentifier("size");
    public static final FIdentifier C_ARRAY = new FIdentifier("c_array");
    public static final FIdentifier COPY = new FIdentifier("copy");

    private FArray(FIdentifier newIdentifier, FClass baseClass, ImmutableList<FType> instantiatedParameters) {
        super(newIdentifier, baseClass, instantiatedParameters);
    }

    public static FArray getArrayFrom(FType baseType) {
        try {
            return (FArray) FBaseArray.F_INSTANCE.getInstantiation(ImmutableList.of(baseType));
        } catch (WrongNumberOfTypeArguments wrongNumberOfTypeArguments) {
            return Utils.cantHappen();
        }
    }

    public FType getBaseType() {
        return Iterables.getOnlyElement(getParametersList());
    }

    public FFunction getArrayGet() {
        return getInstantiatedFunction(FBaseArray.F_INSTANCE.access.a);
    }
    public FFunction getArraySet() {
        return getInstantiatedFunction(FBaseArray.F_INSTANCE.access.b);
    }

    public FField getSize() {
        return getInstanceFields().get(SIZE);
    }




    static class FBaseArray extends FPredefinedClass {
        /*
            The current "simple" choreographing of instantiating the array classes only works, because I give both of
            them the identical TypeVariable as parameter.
            This is potentially dangerous, because it can't happen usually and may lead to unexpected behaviour.
            If that should happen, an even more complicated instantiation Dance needs to be performed, because both
            constructors need an instantiation of the other array class to build all funciton headers.
            For this to happen, the parser needs to be in an early enough stage, such that the "prepare" step of
            instantiated functions is NOT immediately called. (this usually should be the case but is hard to enforce)
         */
        static final FBaseArray F_INSTANCE = new FBaseArray(FTypeVariable.create(new FIdentifier("baseType"), true));
        static final CArray.CBaseArray C_INSTANCE = new CArray.CBaseArray(F_INSTANCE);

        private Pair<FFunction, FFunction> access;

        //TODO @PositionForGeneratedCode
        FBaseArray(FTypeVariable baseType) {
            super(new FArrayIdentifier(baseType.getIdentifier()));
            setParameters(List.of(baseType), List.of(Variance.Invariant));
        }

        void init(CArray.CBaseArray cArray) {
            FTypeVariable baseType = getBase();
            DefaultNamespace namespace = getNamespace();

            //addDefaultFunctions(); TODO should only be added once for "base class"
            //TODO add container equals, and prolly do something to equality once that is done
            InstanceField size = new InstanceField(null, SIZE, FIntN._32, this, FVisibilityModifier.EXPORT, false);
            addFieldTrusted(size); //TODO make final
            namespace.getFunctions(true).get(size.getIdentifier()).clear(); //remove setter TODO no longer needed when field is final
            access = Access.createPredefined(this, baseType, FIntN._32);
            namespace.addFunctionTrusted(access.a);
            namespace.addFunctionTrusted(access.b);

            FunctionBuilder builder = new FunctionBuilder().setMemberOf(namespace).setPredefined(true);

            builder.setIdentifier(C_ARRAY);
            namespace.addFunctionTrusted(builder.setParams(this).setReturnType(cArray).build());

            builder.setIdentifier(COPY);
            ImmutableList<FParameter> params = ImmutableList.of(
                    FParameter.create(new FIdentifier("source"), this, false),
                    FParameter.create(new FIdentifier("target"), this, false),
                    FParameter.create(new FIdentifier("sourceOffset"), FIntN._32, true),
                    FParameter.create(new FIdentifier("targetOffset"), FIntN._32, true),
                    FParameter.create(new FIdentifier("length"), FIntN._32, true)
            );
            params.get(2).setDefaultValueTrusted(new FLiteralExpression(null, new FIntNLiteral(0)), emptySet());
            params.get(3).setDefaultValueTrusted(new FLiteralExpression(null, new FIntNLiteral(0)), emptySet());
            FFunctionCall sourceSize = FFunctionCall.createTrusted(null, size.getGetter().getSignature(), mutableSingletonList(new FVariableExpression(null, params.get(0))));
            params.get(4).setDefaultValueTrusted(sourceSize, Set.of(params.get(0)));
            namespace.addFunctionTrusted(builder.setParams(params).setReturnType(FTuple.VOID).build());

            setConstructorVisibility(FVisibilityModifier.EXPORT);
            namespace.addFunctionTrusted(FConstructor.create(FVisibilityModifier.EXPORT, this, true));
            setForImpl(new ForByIdx(access.a, size.getGetter()));
        }

        protected FTypeVariable getBase() {
            return Iterables.getOnlyElement(getParametersList());
        }



        @Override
        protected FInstantiatedClass createInstantiatedClass(ImmutableList<FType> args) {
            return new FArray(new FArrayIdentifier(Iterables.getOnlyElement(args).getIdentifier()), this, args);
        }
    }
}
