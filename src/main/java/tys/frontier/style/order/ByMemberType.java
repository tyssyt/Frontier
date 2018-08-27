package tys.frontier.style.order;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FTypeMember;
import tys.frontier.code.FTypeMember.MemberType;
import tys.frontier.util.EnumComparator;

import java.util.Comparator;

public class ByMemberType implements Comparator<FTypeMember> {

    public static final ByMemberType DEFAULT = new ByMemberType(new EnumComparator<>(MemberType.class,
            ImmutableList.<MemberType>builder()
                .add(MemberType.FIELD)
                .add(MemberType.CONSTRUCTOR)
                .add(MemberType.OPERATOR)
                .add(MemberType.FUNCTION)
                .build()
    ));

    private Comparator<MemberType> memberOrder;

    public ByMemberType(Comparator<MemberType> memberOrder) {
        this.memberOrder = memberOrder;
    }

    @Override
    public int compare(FTypeMember o1, FTypeMember o2) {
        return memberOrder.compare(o1.getMemberType(), o2.getMemberType());
    }
}
