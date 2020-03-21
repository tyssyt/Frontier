package tys.frontier.code.predefinedClasses;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FField;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.expression.FNamespaceExpression;
import tys.frontier.code.function.FBaseFunction;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FReturn;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.code.type.FTypeVariable;

import static java.util.Collections.singletonMap;

public class FTypeType extends FBaseClass {

    public static final FIdentifier IDENTIFIER = new FIdentifier("!Type");

    public static final FTypeType INSTANCE = new FTypeType();

    public static final FIdentifier typeof_ID = new FIdentifier("of");
    public static final FFunction typeOf;

    //static fields
    public static FIdentifier allTypes_ID = new FIdentifier("allTypes");
    public static final FField allTypes;

    //instance fields
    public static final FField name;

    static {
        name = new FField(new FIdentifier("name"), FStringLiteral.TYPE, INSTANCE, FVisibilityModifier.EXPORT, false, false);
        INSTANCE.addFieldTrusted(name); //TODO make final

        FArray typeTypeArray = FArray.getArrayFrom(INSTANCE);
        allTypes = new FField(allTypes_ID, typeTypeArray, INSTANCE, FVisibilityModifier.EXPORT, true, false);
        INSTANCE.addFieldTrusted(allTypes); //TODO make final

        FTypeVariable t = FTypeVariable.create(new FIdentifier("T"), true);
        ImmutableList<FParameter> of = ImmutableList.of(FParameter.create(FIdentifier.THIS, t, false));
        DefaultNamespace namespace = INSTANCE.getNamespace();
        typeOf = new FBaseFunction(typeof_ID, namespace, FVisibilityModifier.EXPORT, false, FTypeType.INSTANCE, of, null, singletonMap(t.getIdentifier(), t));
        typeOf.setBody(FBlock.from(FReturn.createTrusted(new FNamespaceExpression(t.getNamespace()), typeOf)));
        namespace.addFunctionTrusted(typeOf);
    }


    private FTypeType() {
        super(IDENTIFIER, FVisibilityModifier.EXPORT, false);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}
