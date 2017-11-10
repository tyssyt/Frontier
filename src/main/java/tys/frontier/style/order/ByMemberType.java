package tys.frontier.style.order;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FClassMember;
import tys.frontier.code.FClassMember.MemberType;
import tys.frontier.util.EnumComparator;

import java.util.Comparator;

public class ByMemberType implements Comparator<FClassMember> {

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
    public int compare(FClassMember o1, FClassMember o2) {
        return memberOrder.compare(o1.getMemberType(), o2.getMemberType());
    }
}
