package tys.frontier.util;

import java.util.ListIterator;
import java.util.function.Function;

public class TransformedListIterator<F, T> implements ListIterator<T> {

    private final ListIterator<F> base;
    private final Function<F, T> function;

    public TransformedListIterator(ListIterator<F> base, Function<F, T> function) {
        this.base = base;
        this.function = function;
    }

    @Override
    public boolean hasNext() {
        return base.hasNext();
    }

    @Override
    public T next() {
        return function.apply(base.next());
    }

    @Override
    public boolean hasPrevious() {
        return base.hasPrevious();
    }

    @Override
    public T previous() {
        return function.apply(base.previous());
    }

    @Override
    public int nextIndex() {
        return base.nextIndex();
    }

    @Override
    public int previousIndex() {
        return base.previousIndex();
    }

    @Override
    public void remove() {
        base.remove();
    }

    @Override
    public void set(T t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void add(T t) {
        throw new UnsupportedOperationException();
    }
}
