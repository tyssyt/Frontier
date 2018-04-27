package tys.frontier.code;

import tys.frontier.code.identifier.IdentifierNameable;

public interface FClassMember extends IdentifierNameable, HasVisibility {

    enum MemberType {
        FIELD,
        CONSTRUCTOR,
        OPERATOR,
        FUNCTION
    }

    boolean isStatic();
    MemberType getMemberType();

}
