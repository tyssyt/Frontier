package tys.frontier.style.order;

import tys.frontier.code.FTypeMember;

import java.util.Comparator;

public class Alphabetical implements Comparator<FTypeMember> {

    public static final Alphabetical INSTANCE = new Alphabetical();

    private Alphabetical() {}

    @Override
    public int compare(FTypeMember o1, FTypeMember o2) {
        return o1.getIdentifier().name.compareTo(o2.getIdentifier().name);
    }
}
