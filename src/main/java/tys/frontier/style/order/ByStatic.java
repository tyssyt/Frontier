package tys.frontier.style.order;

import tys.frontier.code.FClassMember;

import java.util.Comparator;

public class ByStatic implements Comparator<FClassMember> {

    public static final ByStatic PREFER_STATIC = new ByStatic(true);
    public static final ByStatic PREFER_INSTANCE = new ByStatic(false);

    private boolean preferStatic;

    private ByStatic(boolean preferStatic) {
        this.preferStatic = preferStatic;
    }

    @Override
    public int compare(FClassMember o1, FClassMember o2) {
        return value(o1) - value(o2);
    }

    private int value(FClassMember m) {
        return m.isStatic() ^ preferStatic ? 0 : 1;
    }
}
