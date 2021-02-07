package tys.frontier.code.expression;

import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.util.Joiners;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static tys.frontier.util.Utils.typesFromExpressionList;

public class FArrayLiteral extends FExpression {

    private FArray type;
    private List<FExpression> elements;

    private FArrayLiteral(Position position, FType elementType, List<FExpression> elements) throws IncompatibleTypes {
        super(position);
        this.type = FArray.getArrayFrom(elementType);
        this.elements = elements;
        checkTypes(elementType);
    }

    public static FArrayLiteral create(Position position, FType elementType, List<FExpression> elements) throws IncompatibleTypes {
        return new FArrayLiteral(position, elementType, elements);
    }

    public static FArrayLiteral createTrusted(Position position, FType elementType, List<FExpression> elements) {
        try {
            return create(position, elementType, elements);
        } catch (IncompatibleTypes incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    @Override
    public FArray getType() {
        return type;
    }

    public List<FExpression> getElements() {
        return elements;
    }

    public int getSize() {
        return elements.size() / FTuple.arity(type.getBaseType());
    }

    private void checkTypes(FType elementType) throws IncompatibleTypes {
        List<FType> fTypes = FTuple.unpackType(elementType);

        int typeIdx = 0;
        for (int i = 0; i < elements.size(); i++) {
            elements.set(i, elements.get(i).typeCheck(fTypes.get(typeIdx)));
            typeIdx = (typeIdx + 1) % fTypes.size();
        }

        if (elements.size() % fTypes.size() != 0) {
            int sIdx = elements.size() - (elements.size() % fTypes.size());
            Position p1 = elements.get(sIdx).getPosition();
            Position p2 = elements.get(elements.size()-1).getPosition();
            Position pos = new Position(p1.getLineFrom(), p2.getLineTo(), p1.getColumnFrom(), p2.getColumnTo());
            throw new IncompatibleTypes(pos, elementType, FTuple.from(typesFromExpressionList(elements.subList(sIdx, elements.size()))));
        }
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        visitor.enterArrayLiteral(this);
        List<E> elements = new ArrayList<>(this.elements.size());
        for (FExpression element : this.elements)
            elements.add(element.accept(visitor));
        return visitor.exitArrayLiteral(this, elements);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitArrayLiteral(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append('[').append(type.getIdentifier().name).append(": ");
        return Joiners.ON_COMMA.appendTo(sb, elements).append(']');
    }
}
