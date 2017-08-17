package tys.frontier.code.statement.loop;

import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FStatement;
import tys.frontier.code.statement.FVarDeclaration;

public class FFor implements FLoop {

    FVarDeclaration declaration; //optional
    FExpression condition; //optional
    FExpression incement; //optional
    FStatement body;

}
