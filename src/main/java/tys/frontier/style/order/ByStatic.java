package tys.frontier.style.order;

import tys.frontier.code.FTypeMember;

import java.util.Comparator;

public class ByStatic implements Comparator<FTypeMember> {

    public static final ByStatic PREFER_STATIC = new ByStatic(true);
    public static final ByStatic PREFER_INSTANCE = new ByStatic(false);

    private boolean preferStatic;

    private ByStatic(boolean preferStatic) {
        this.preferStatic = preferStatic;
    }

    @Override
    public int compare(FTypeMember o1, FTypeMember o2) {
        return value(o1) - value(o2);
    }

    private int value(FTypeMember m) {
        return m.isInstance() ^ preferStatic ? 1 : 0;
    }
}
