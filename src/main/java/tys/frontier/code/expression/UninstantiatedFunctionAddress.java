package tys.frontier.code.expression;

import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.predefinedClasses.FFunctionType;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.type.FType;
import tys.frontier.code.visitor.ExpressionVisitor;
import tys.frontier.code.visitor.ExpressionWalker;
import tys.frontier.parser.location.Position;
import tys.frontier.util.Utils;

import java.util.List;

public class UninstantiatedFunctionAddress extends FExpression {

    private FFunction uninstantiatedFunction;
    private TypeInstantiation typeInstantiation;
    private FFunctionType type;

    public UninstantiatedFunctionAddress(Position position, FFunction uninstantiatedFunction) {
        super(position);
        this.uninstantiatedFunction = uninstantiatedFunction;
        typeInstantiation = FFunctionType.copyVarsForInstantiation(uninstantiatedFunction);

        List<FType> in = Utils.typesFromExpressionList(uninstantiatedFunction.getSignature().getParameters(), typeInstantiation::getType);
        type = FFunctionType.from(FTuple.from(in), typeInstantiation.getType(uninstantiatedFunction.getType()));
    }

    @Override
    public FFunctionType getType() {
        return type;
    }

    public FFunction getUninstantiatedFunction() {
        return uninstantiatedFunction;
    }

    public TypeInstantiation getTypeInstantiation() {
        return typeInstantiation;
    }

    @Override
    public <E> E accept(ExpressionVisitor<E> visitor) {
        return visitor.visitUninstantiatedFunctionAddress(this);
    }

    @Override
    public <E> E accept(ExpressionWalker<E> walker) {
        return walker.visitUninstantiatedFunctionAddress(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) { //TODO print types instead of identifiers of params
        sb.append(uninstantiatedFunction.getIdentifier()).append('(');
        return Utils.joinIdentifiers(sb, uninstantiatedFunction.getSignature().getParameters(), ",").append(")*");
    }
}
