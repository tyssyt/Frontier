package tys.frontier.parser.syntaxErrors;

import tys.frontier.code.namespace.Namespace;

public class NamespaceWithoutType extends TypeNotFound {

    public final Namespace namespace;

    public NamespaceWithoutType(Namespace namespace) {
        super(namespace.getIdentifier());
        this.namespace = namespace;
    }
}