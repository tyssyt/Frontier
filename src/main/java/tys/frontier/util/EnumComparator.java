package tys.frontier.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class EnumComparator<E extends Enum<E>> implements Comparator<E> {

    private int[] order;

    public EnumComparator(Class<E> enumClass) {
        this(enumClass, Arrays.asList(enumClass.getEnumConstants()));
    }

    public EnumComparator(Class<E> keyType, List<E> order) {
        this.order = new int[keyType.getEnumConstants().length];
        int weight = order.size();
        for (E e : order) {
            this.order[e.ordinal()] = --weight;
        }
    }

    @Override
    public int compare(E o1, E o2) {
        return order[o1.ordinal()] - order[o2.ordinal()];
    }

    public static class Builder<E extends Enum<E>> {
        Class<E> keyType;
        List<E> order = new ArrayList<>();

        public Builder(Class<E> keyType) {
            this.keyType = keyType;
        }

        public Builder next(E e) {
            if (!order.contains(e))
                order.add(e);
            return this;
        }

        public EnumComparator<E> build() {
            return new EnumComparator<>(keyType, order);
        }
    }
}