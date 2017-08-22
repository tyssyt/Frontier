package tys.frontier.code.Operator;

import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.expression.FFunctionCall;
import tys.frontier.code.expression.FVariableExpression;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.statement.FReturn;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

//weird exception of pre-definedness: do not print it to .front, but handle it normal in backend
public class FNotEquals extends FOperator {
    public FNotEquals(FEquals equals) {
        super(FFunctionIdentifier.NOT_EQUALS, equals.getClazz(), equals.getType(), equals.getParams());
        List<FExpression> params = new ArrayList<>(equals.getParams().size());
        for (FLocalVariable var : equals.getParams()) {
            params.add(new FVariableExpression(var));
        }
        body = Collections.singletonList(
                new FReturn(
                        new FFunctionCall(
                                FBool.INSTANCE.getFunction(
                                        new Signature(FFunctionIdentifier.NOT, Collections.singletonList(FBool.INSTANCE))
                                ),
                                Collections.singletonList(new FFunctionCall(
                                        equals, params
                                ))
                        ),
                        this
                )
        );
    }
}
