package tys.frontier.code.selector;

import tys.frontier.code.identifier.FIdentifier;

import java.util.Collection;

public class SelectIn<I extends FIdentifier> implements Selector<I> {

    private Collection<I> identifiers;

    SelectIn(Collection<I> identifiers) {
        this.identifiers = identifiers;
    }

    @Override
    public boolean has(I identifier) {
        return identifiers.contains(identifier);
    }

    @Override
    public void select(Collection<I> collection) {
        collection.retainAll(identifiers);
    }
}
