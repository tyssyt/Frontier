package tys.frontier.code;

import tys.frontier.code.identifier.IdentifierNameable;

public interface FClassMember extends IdentifierNameable {

    enum MemberType {
        FIELD,
        CONSTRUCTOR,
        OPERATOR,
        FUNCTION
    }

    boolean isStatic();
    FVisibilityModifier getModifier();
    MemberType getMemberType();

}
