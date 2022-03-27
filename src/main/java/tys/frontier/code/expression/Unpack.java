package tys.frontier.code.expression;

import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Unpack extends FExpression {

    private FExpression unpackedExpression;
    private List<UnpackedElement> unpackedElements;

    public Unpack(FExpression unpackedExpression) {
        super(unpackedExpression.getPosition());
        assert FTuple.arity(unpackedExpression.getType()) > 1;
        assert !(unpackedExpression instanceof Pack);
        this.unpackedExpression = unpackedExpression;
        FTuple tuple = (FTuple) unpackedExpression.getType();
        this.unpackedElements = new ArrayList<>(tuple.arity());
        int lastIdx = tuple.arity() - 1;
        for (int i = 0; i < lastIdx; i++) {
            unpackedElements.add(new UnpackedElement(i, tuple.getTypes().get(i), false));
        }
        unpackedElements.add(new UnpackedElement(lastIdx, tuple.getTypes().get(lastIdx), true));
    }

    @Override
    public FType getType() {
        return unpackedExpression.getType();
    }

    public FExpression getUnpackedExpression() {
        return unpackedExpression;
    }

    public List<UnpackedElement> getUnpackedElements() {
        return unpackedElements;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterUnpack(this);
        return visitor.exitUnpack(this, unpackedExpression.accept(visitor));
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitUnpack(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append("unpack(");
        return unpackedExpression.toString(sb).append(')');
    }

    public class UnpackedElement extends FExpression {
        private int index;
        private FType type;
        private boolean isLast;

        public UnpackedElement(int index, FType type, boolean isLast) {
            super(null); //TODO @PositionForGeneratedCode
            this.index = index;
            this.type = type;
            this.isLast = isLast;
        }

        @Override
        public FType getType() {
            return type;
        }

        public Unpack getUnpack() {
            return Unpack.this;
        }

        public boolean isFirst() {
            return index == 0;
        }

        public boolean isLast() {
            return isLast;
        }

        public int getIndex() {
            return index;
        }

        @Override
        public <E> E accept(ExpressionVisitor<E> visitor) {
            boolean visitUnpack = visitor.enterUnpackedElement(this);
            Optional<E> unpack = visitUnpack ? Optional.of(getUnpack().accept(visitor)) : Optional.empty();
            return visitor.exitUnpackedElement(this, unpack);
        }

        @Override
        public <E> E accept(ExpressionWalker<E> walker) {
            return walker.visitUnpackedElement(this);
        }

        @Override
        public StringBuilder toString(StringBuilder sb) {
            return sb.append(isLast ? index + " of " + unpackedExpression : index + " of unpack");
        }
    }

}
