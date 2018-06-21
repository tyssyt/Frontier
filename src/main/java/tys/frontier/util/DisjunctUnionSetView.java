package tys.frontier.util;

import com.google.common.collect.Iterables;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Stream;

public class DisjunctUnionSetView<T> extends AbstractSet<T> {

    private Collection<Set<T>> sets;

    protected DisjunctUnionSetView(Collection<Set<T>> sets) {
        this.sets = sets;
    }

    @SafeVarargs
    public static <T> Set<T> of(Set<T>... sets) {
        return of(Arrays.asList(sets));
    }

    public static <T> Set<T> of(Collection<Set<T>> sets) {
        if (sets.isEmpty())
            return Collections.emptySet();
        if (sets.size() == 1)
            return Iterables.getOnlyElement(sets);
        return new DisjunctUnionSetView<>(sets);
    }

    @Override
        public int size() {
            int size = 0;
            for (Set<T> set : sets) {
                size += set.size();
            }
            return size;
        }

        @Override
        public boolean contains(Object o) {
            for (Set<T> set : sets) {
                if (set.contains(o))
                    return true;
            }
            return false;
        }

        @Override
        public boolean remove(Object o) {
            boolean changed = false;
            for (Set<T> set : sets) {
                changed |= set.remove(o);
            }
            return changed;
        }

        @Override
        public boolean retainAll(Collection<?> c) {
            boolean changed = false;
            for (Set<T> set : sets) {
                changed |= set.retainAll(c);
            }
            return changed;
        }

        @Override
        public void clear() {
            for (Set<T> set : sets) {
                set.clear();
            }
        }

        @Override
        public Iterator<T> iterator() {
            return new Iterator<T>() {
                Iterator<Set<T>> metaIt = sets.iterator();
                Iterator<? extends T> it = metaIt.next().iterator();

                @Override
                public boolean hasNext() {
                    return it.hasNext() || metaIt.hasNext();
                }

                @Override
                public T next() {
                    if (!it.hasNext()) {
                        it = metaIt.next().iterator();
                    }
                    return it.next();
                }

                @Override
                public void remove() {
                    it.remove();
                }
            };
        }

        @Override
        public Stream<T> stream() {
            return sets.stream().flatMap(Set::stream);
        }

        @Override
        public Stream<T> parallelStream() {
            return sets.stream().parallel().flatMap(Set::stream);
        }

        @Override
        public void forEach(Consumer<? super T> action) {
            for (Set<T> set : sets) {
                set.forEach(action);
            }
        }
}
