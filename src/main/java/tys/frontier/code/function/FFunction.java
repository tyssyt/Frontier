package tys.frontier.code.function;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.TypeInstantiation;
import tys.frontier.code.Typed;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.predefinedClasses.FTuple;
import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.util.StringBuilderToString;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface FFunction extends IdentifierNameable, Typed, ControlFlowIDontKnow, StringBuilderToString {

    default boolean isInstance() {
        return getSignature().isInstance();
    }

    Namespace getMemberOf();

    FVisibilityModifier getVisibility();

    NativeDecl getNative();

    Signature getSignature();
    Signature getLhsSignature();

    Optional<FBlock> getBody();

    void setBody(FBlock body);

    @Override
    FIdentifier getIdentifier();

    @Override
    default FType getType() {
        return getSignature().getType();
    }

    boolean isConstructor();

    boolean isPredefined();

    boolean isMain();

    Location getLocation();

    FLocalVariable getFreshVariable(Position position, FType type);

    Map<FIdentifier, FTypeVariable> getParameters();

    List<FTypeVariable> getParametersList(); //TODO see if we can eliminate either this or the map

    boolean isInstantiation();
    FFunction getBaseR();
    TypeInstantiation getTypeInstantiationToBase();

    FFunction getInstantiation(TypeInstantiation typeInstantiation);

    default <N,C,Fi,Fu,S,E> Fu accept(ClassVisitor<N,C, Fi, Fu, S, E> visitor) {
        visitor.enterFunction(this);
        return visitor.exitFunction(this, getBody().map(body -> body.accept(visitor)));
    }

    default String headerToString() {
        return (getBody().isPresent() ? "" : "abstract ") + getVisibility() + " " + getIdentifier() + " " + getSignature().getParameters() + (getType() == FTuple.VOID ? "" : " -> " +  getType().getIdentifier());
    }

    @Override
    default StringBuilder toString(StringBuilder sb) {
        sb.append(headerToString()).append(" {\n");
        if (isPredefined())
            sb.append("predefined");
        else
            if (getBody().isPresent())
            for (FStatement statement : getBody().get())
                statement.toString(sb).append('\n');
        return sb.append('}');
    }

}
