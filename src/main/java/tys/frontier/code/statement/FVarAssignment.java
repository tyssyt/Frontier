package tys.frontier.code.statement;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.function.operator.FBinaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.type.FClass;
import tys.frontier.code.visitor.StatementVisitor;
import tys.frontier.code.visitor.StatementWalker;
import tys.frontier.parser.syntaxErrors.IncompatibleTypes;
import tys.frontier.parser.syntaxErrors.NotEnoughArguments;
import tys.frontier.parser.syntaxErrors.TooManyArguments;
import tys.frontier.parser.syntaxErrors.UnfulfillableConstraints;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static com.google.common.collect.ImmutableMap.toImmutableMap;
import static java.util.Collections.singletonList;
import static tys.frontier.code.function.operator.FBinaryOperator.Arith.*;

public class FVarAssignment implements FStatement {

    private List<FVariableExpression> variables;
    private Operator operator;
    private List<FExpression> values;
    private ArgMapping argMapping;

    public FVarAssignment(List<FVariableExpression> variables, Operator operator, List<FExpression> values, ArgMapping argMapping) {
        assert operator == Operator.ASSIGN || variables.stream().noneMatch(v -> v instanceof FVarDeclaration);
        this.variables = variables;
        this.operator = operator;
        this.values = values;
        this.argMapping = argMapping;
        for (FVariableExpression variable : variables) {
            variable.setAccessType(FVariableExpression.AccessType.STORE);
        }
    }

    public static FVarAssignment create(List<FVariableExpression> variables, Operator operator, List<FExpression> values) throws IncompatibleTypes, TooManyArguments, NotEnoughArguments, UnfulfillableConstraints {
        ArgMapping argMap = ArgMapping.createCasted(Utils.typesFromExpressionList(values), Utils.typesFromExpressionList(variables));
        return new FVarAssignment(variables, operator, values, argMap);
    }

    public static FVarAssignment createTrusted(List<FVariableExpression> variables, Operator operator, List<FExpression> values) {
        try {
            return create(variables, operator, values);
        } catch (IncompatibleTypes | TooManyArguments | NotEnoughArguments | UnfulfillableConstraints incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public static FVarAssignment createDecl(FLocalVariable variable, FExpression value) {
        try {
            ArgMapping argMapping = ArgMapping.createBasic(singletonList(value.getType()), singletonList(variable.getType()));
            return new FVarAssignment(singletonList(new FVarDeclaration(variable)), Operator.ASSIGN, Arrays.asList(value), argMapping);
        } catch (IncompatibleTypes | UnfulfillableConstraints incompatibleTypes) {
            return Utils.cantHappen();
        }
    }

    public List<FVariableExpression> getVariables() {
        return variables;
    }

    public Operator getOperator() {
        return operator;
    }

    public List<FExpression> getValues() {
        return values;
    }

    public ArgMapping getArgMapping() {
        return argMapping;
    }

    @Override
    public Optional<ControlFlowIDontKnow> redirectsControlFlow() {
        return Optional.empty();
    }

    @Override
    public <S, E> S accept(StatementVisitor<S, E> visitor) {
        visitor.enterVarAssignment(this);
        List<E> variables = new ArrayList<>(this.variables.size());
        for (FExpression v : this.variables)
            variables.add(v.accept(visitor));
        List<E> values = new ArrayList<>(this.values.size());
        for (FExpression v : this.values)
            values.add(v.accept(visitor));
        return visitor.exitVarAssignment(this, variables, values);
    }

    @Override
    public <S, E> S accept(StatementWalker<S, E> walker) {
        return walker.visitVarAssignment(this);
    }

    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb.append(Joiner.on(", ").join(variables));
        sb.append(' ').append(operator).append(' ');
        sb.append(Joiner.on(", ").join(values));
        return sb.append(';');
    }
    @Override
    public String toString() {
        return tS();
    }

    public enum Operator { //TODO type restrictions of operators
        ASSIGN("=", null),
        ADD_ASSIGN("+=", PLUS.identifier),
        SUB_ASSIGN("-=", MINUS.identifier),
        MUL_ASSIGN("*=", TIMES.identifier),
        DIV_ASSIGN("/=", DIVIDED.identifier),
        AND_ASSIGN("&=", AND.identifier),
        OR_ASSIGN("|=", OR.identifier),
        XOR_ASSIGN("^=", XOR.identifier),
        MOD_ASSIGN("%=", MODULO.identifier);

        private static ImmutableMap<String, Operator> stringMap =
                Arrays.stream(values()).collect(toImmutableMap(o -> o.stringRepresentation, o -> o));

        public final String stringRepresentation;
        public final FFunctionIdentifier identifier;

        Operator(String s, FFunctionIdentifier id) {
            stringRepresentation = s;
            identifier = id;
        }

        public FBinaryOperator getOperator(FClass fClass) {
            return (FBinaryOperator) Iterables.getOnlyElement(fClass.getFunctions().get(identifier));
        }

        public static Operator fromString (String string) {
            return stringMap.get(string);
        }

        @Override
        public String toString() {
            return stringRepresentation;
        }
    }
}
