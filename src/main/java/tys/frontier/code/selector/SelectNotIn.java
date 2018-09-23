package tys.frontier.code.selector;

import tys.frontier.code.identifier.FIdentifier;

import java.util.Collection;

public class SelectNotIn<I extends FIdentifier> implements Selector<I> {

    private Collection<I> identifiers;

    SelectNotIn(Collection<I> identifiers) {
        this.identifiers = identifiers;
    }

    @Override
    public boolean has(I identifier) {
        return !identifiers.contains(identifier);
    }

    @Override
    public void select(Collection<I> collection) {
        collection.removeAll(identifiers);
    }
}
