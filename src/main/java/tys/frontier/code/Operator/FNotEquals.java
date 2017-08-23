package tys.frontier.code.Operator;

import com.google.common.collect.ImmutableList;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FReturn;
import tys.frontier.parser.syntaxTree.syntaxErrors.StaticAccessToInstanceFunction;

import java.util.ArrayList;
import java.util.List;

//weird exception of pre-definedness: do not print it to .front, but handle it normal in backend
public class FNotEquals extends FOperator {
    public FNotEquals(FEquals equals) {
        super(FFunctionIdentifier.NOT_EQUALS, equals.getClazz(), equals.getType(), equals.getParams());
        List<FExpression> params = new ArrayList<>(equals.getParams().size());
        for (FLocalVariable var : equals.getParams()) {
            params.add(new FVariableExpression(var));
        }
        try {
            body = ImmutableList.of(
                    new FReturn(
                            new FFunctionCall(
                                    FBool.INSTANCE.getFunction(
                                            new Signature(FFunctionIdentifier.NOT, ImmutableList.of(FBool.INSTANCE))
                                    ),
                                    ImmutableList.of(new FFunctionCall(
                                            equals, params
                                    ))
                            ),
                            this
                    )
            );
        } catch (StaticAccessToInstanceFunction e) {
            throw new RuntimeException(e); //something went terribly wrong
        }
    }
}
