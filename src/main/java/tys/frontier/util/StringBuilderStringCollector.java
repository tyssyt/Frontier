package tys.frontier.util;

import com.google.common.collect.ImmutableSet;

import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

public class StringBuilderStringCollector<T> implements Collector<T, StringBuilder, String> {
    private final BiConsumer<StringBuilder, T> accumulator;

    public StringBuilderStringCollector(BiConsumer<StringBuilder, T> accumulator) {
        this.accumulator = accumulator;
    }

    @Override
    public Supplier<StringBuilder> supplier() {
        return StringBuilder::new;
    }

    @Override
    public BiConsumer<StringBuilder, T> accumulator() {
        return accumulator;
    }

    @Override
    public BinaryOperator<StringBuilder> combiner() {
        return StringBuilder::append;
    }

    @Override
    public Function<StringBuilder, String> finisher() {
        return StringBuilder::toString;
    }

    @Override
    public Set<Characteristics> characteristics() {
        return ImmutableSet.of(Characteristics.CONCURRENT);
    }
}
