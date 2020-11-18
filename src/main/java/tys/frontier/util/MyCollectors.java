package tys.frontier.util;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.collectingAndThen;

public final class MyCollectors {

    private MyCollectors() {}

    public static <T> Collector<T, ?, Pair<List<T>, List<T>>> partitioningBy(Predicate<? super T> predicate) {
        return collectingAndThen(Collectors.partitioningBy(predicate), partition -> new Pair<>(partition.get(true), partition.get(false)));
    }

}
