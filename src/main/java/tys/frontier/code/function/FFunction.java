package tys.frontier.code.function;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.*;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.identifier.FTypeIdentifier;
import tys.frontier.code.identifier.IdentifierNameable;
import tys.frontier.code.statement.ControlFlowIDontKnow;
import tys.frontier.code.statement.FBlock;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.visitor.ClassVisitor;
import tys.frontier.util.StringBuilderToString;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface FFunction extends FTypeMember, HasTypeParameters<FFunction>, IdentifierNameable, Typed, ControlFlowIDontKnow, StringBuilderToString {

    @Override
    default boolean isInstance() {
        return getParams().size() > 0 && getParams().get(0).getType() == getMemberOf() && !getParams().get(0).hasDefaultValue();
    }

    @Override
    FType getMemberOf();

    @Override
    FVisibilityModifier getVisibility();

    boolean isNative();

    ImmutableList<FParameter> getParams();

    boolean addCall(FFunctionCall call);

    List<FFunctionCall> getCalledBy();

    Optional<FBlock> getBody();

    void setBody(FBlock body);

    @Override
    FFunctionIdentifier getIdentifier();

    @Override
    FType getType();

    @Override
    default MemberType getMemberType() {
        return MemberType.FUNCTION;
    }

    boolean isConstructor();

    boolean isPredefined();

    Signature getSignature();

    boolean isMain();

    FLocalVariable getFreshVariable(FType type);

    @Override
    Map<FTypeIdentifier, FTypeVariable> getParameters();

    @Override
    List<FTypeVariable> getParametersList();

    boolean isInstantiation();
    FFunction getBaseR();
    TypeInstantiation getTypeInstantiationToBase();

    @Override
    FFunction getInstantiation(TypeInstantiation typeInstantiation);

    default <C,Fi,Fu,S,E> Fu accept(ClassVisitor<C, Fi, Fu, S, E> visitor) {
        visitor.enterFunction(this);
        return visitor.exitFunction(this, getBody().map(body -> body.accept(visitor)));
    }

    default String headerToString() {
        return (getBody().isPresent() ? "" : "abstract ") + getVisibility() + " " +  getType().getIdentifier() + " " + getIdentifier() + " " + getParams();
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
