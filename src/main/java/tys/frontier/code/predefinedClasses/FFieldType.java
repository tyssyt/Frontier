package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FField;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.type.FBaseClass;
import tys.frontier.passes.analysis.reachability.Reachability;

public class FFieldType extends FBaseClass {

    //TODO we could make it a generic class and instantiate the generic with the class we are in?

    public static final FIdentifier IDENTIFIER = new FIdentifier("Field");

    public static final FFieldType INSTANCE = new FFieldType();

    //instance fields
    public static final FField name;
    public static final FField type;
    public static final FField memberOf;

    static {
        //field name
        {
            name = new FField(new FIdentifier("name"), FStringLiteral.TYPE, INSTANCE, FVisibilityModifier.EXPORT, false, false);
            INSTANCE.addFieldTrusted(name); //TODO make final
        }
        //field type
        {
            type = new FField(new FIdentifier("type"), FTypeType.INSTANCE, INSTANCE, FVisibilityModifier.EXPORT, false, false);
            INSTANCE.addFieldTrusted(type); //TODO make final
        }
        //field memberOf
        {
            memberOf = new FField(new FIdentifier("memberOf"), FTypeType.INSTANCE, INSTANCE, FVisibilityModifier.EXPORT, false, false);
            INSTANCE.addFieldTrusted(memberOf); //TODO make final
        }

        INSTANCE.generateConstructor();
    }


    private FFieldType() {
        super(IDENTIFIER, FVisibilityModifier.EXPORT, false);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }

    @Override
    public void removeUnreachable(Reachability.ReachableNamespace reachable) {}
}
