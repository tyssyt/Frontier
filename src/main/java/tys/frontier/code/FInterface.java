package tys.frontier.code;

import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.parser.syntaxErrors.IdentifierCollision;
import tys.frontier.parser.syntaxErrors.InterfaceInstanceField;
import tys.frontier.parser.syntaxErrors.InterfaceSuperClass;
import tys.frontier.parser.syntaxErrors.PrivateInterface;

public class FInterface extends FType {

    public FInterface(FTypeIdentifier identifier, FVisibilityModifier visibility) throws PrivateInterface {
        super(identifier, visibility);
        if (visibility == FVisibilityModifier.PRIVATE) {
            throw new PrivateInterface(this);
        }
    }

    @Override
    public String headerToString() {
        return visibility + " interface " + identifier;
    }

    @Override
    public void addField(FField field) throws IdentifierCollision, InterfaceInstanceField {
        if (!field.isStatic()) {
            throw new InterfaceInstanceField(this, field);
        }
        super.addField(field);
    }

    @Override
    protected boolean addSuperClass(FClass superClass) throws InterfaceSuperClass {
        throw new InterfaceSuperClass(this, superClass);
    }
}