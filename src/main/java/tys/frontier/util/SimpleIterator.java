package tys.frontier.util;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Supplier;

//TODO consider implementing PeekingIterator
public class SimpleIterator<E> implements Iterator<E> {

    public SimpleIterator(Supplier<E> computeNext) {
        this.computeNext = computeNext;
        this.next = computeNext.get();
    }

    private Supplier<E> computeNext;
    private E next;

    @Override
    public boolean hasNext() {
        return next != null;
    }

    @Override
    public E next() {
        if (next == null)
            throw new NoSuchElementException();
        E res = next;
        next = computeNext.get();
        return res;
    }
}
