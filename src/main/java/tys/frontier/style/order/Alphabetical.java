package tys.frontier.style.order;

import tys.frontier.code.FClassMember;

import java.util.Comparator;

public class Alphabetical implements Comparator<FClassMember> {

    public static final Alphabetical INSTANCE = new Alphabetical();

    private Alphabetical() {}

    @Override
    public int compare(FClassMember o1, FClassMember o2) {
        return o1.getIdentifier().name.compareTo(o2.getIdentifier().name);
    }
}
