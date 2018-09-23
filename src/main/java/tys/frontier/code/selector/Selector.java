package tys.frontier.code.selector;

import tys.frontier.code.identifier.FIdentifier;

import java.util.Collection;

@FunctionalInterface
public interface Selector<I extends FIdentifier> {

    Selector ALL = new Selector() {
        @Override
        public boolean has(FIdentifier identifier) {
            return true;
        }
        @Override
        public void select(Collection collection) {}
    };

    static <I extends FIdentifier> Selector<I> all() {
        return ALL;
    }

    static <I extends FIdentifier> Selector<I> in(Collection<I> identifiers) {
        return new SelectIn<>(identifiers);
    }

    static <I extends FIdentifier> Selector<I> notIn(Collection<I> identifiers) {
        if (identifiers.isEmpty())
            return ALL;
        return new SelectNotIn<>(identifiers);
    }

    boolean has(I identifier);
    default void select(Collection<I> collection) {
        collection.removeIf(i -> !has(i));
    }

}
