package tys.frontier.code.type;

import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.statement.loop.forImpl.ForImpl;
import tys.frontier.util.StringBuilderToString;

public interface FType extends IdentifierNameable, StringBuilderToString {

    int concreteness(); //TODO there probably is an established term for this

    boolean canImplicitlyCast();

    ForImpl getForImpl();

    Namespace getNamespace();
}
