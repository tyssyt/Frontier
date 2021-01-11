package tys.frontier.code.predefinedClasses;

import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.FStringLiteral;
import tys.frontier.code.type.FBaseClass;

public class FFieldType extends FBaseClass {

    //TODO we could make it a generic class and instantiate the generic with the class we are in?

    public static final FIdentifier IDENTIFIER = new FIdentifier("Field");

    public static final FFieldType INSTANCE = new FFieldType();

    //instance fields
    public static final InstanceField name;
    public static final InstanceField type;
    public static final InstanceField memberOf;

    //TODO @PositionForGeneratedCode, I already have the pseudo file
    static {
        //field name
        {
            name = new InstanceField(null, new FIdentifier("name"), FStringLiteral.TYPE, INSTANCE, FVisibilityModifier.EXPORT, false);
            INSTANCE.addFieldTrusted(name); //TODO make final
        }
        //field type
        {
            type = new InstanceField(null, new FIdentifier("type"), FTypeType.INSTANCE, INSTANCE, FVisibilityModifier.EXPORT, false);
            INSTANCE.addFieldTrusted(type); //TODO make final
        }
        //field memberOf
        {
            memberOf = new InstanceField(null, new FIdentifier("memberOf"), FTypeType.INSTANCE, INSTANCE, FVisibilityModifier.EXPORT, false);
            INSTANCE.addFieldTrusted(memberOf); //TODO make final
        }

        INSTANCE.generateConstructor(false); // primitive for loops over tuples need to generate dynamic Field Info
    }

    private FFieldType() {
        super(null, IDENTIFIER, FVisibilityModifier.EXPORT, null);
    }

    @Override
    public boolean canImplicitlyCast() {
        return false;
    }
}
