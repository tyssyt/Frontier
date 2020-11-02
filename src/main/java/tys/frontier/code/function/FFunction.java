package tys.frontier.code.function;

import tys.frontier.code.*;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.parser.location.Location;
import tys.frontier.util.StringBuilderToString;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface FFunction extends IdentifierNameable, HasVisibility, Typed, ControlFlowIDontKnow, StringBuilderToString {

    default boolean isInstance() {
        return getSignature().isInstance();
    }

    Namespace getMemberOf();

    @Override
    FVisibilityModifier getVisibility();

    boolean isNative();

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

    FLocalVariable getFreshVariable(FType type);

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
        return (getBody().isPresent() ? "" : "abstract ") + getVisibility() + " " +  getType().getIdentifier() + " " + getIdentifier() + " " + getSignature().getParameters();
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
