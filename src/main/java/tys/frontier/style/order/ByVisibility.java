package tys.frontier.style.order;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClassMember;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.util.EnumComparator;

import java.util.Comparator;

public class ByVisibility implements Comparator<FClassMember> {

    public static final ByVisibility DEFAULT = new ByVisibility(new EnumComparator<>(FVisibilityModifier.class,
            ImmutableList.<FVisibilityModifier>builder()
                    .add(FVisibilityModifier.PUBLIC)
                    .add(FVisibilityModifier.NONE)
                    .add(FVisibilityModifier.PRIVATE)
                    .build()
    ));

    private Comparator<FVisibilityModifier> memberOrder;

    public ByVisibility(Comparator<FVisibilityModifier> memberOrder) {
        this.memberOrder = memberOrder;
    }

    @Override
    public int compare(FClassMember o1, FClassMember o2) {
        return memberOrder.compare(o1.getModifier(), o2.getModifier());
    }
}
