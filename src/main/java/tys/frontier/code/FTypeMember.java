package tys.frontier.code;

import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.type.FType;

public interface FTypeMember extends IdentifierNameable, HasVisibility {

    enum MemberType {
        FIELD,
        CONSTRUCTOR,
        OPERATOR,
        FUNCTION
    }

    boolean isInstance();
    MemberType getMemberType();
    FType getMemberOf();

}
