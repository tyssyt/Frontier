package tys.frontier.code.statement.loop;

import tys.frontier.code.FVariable;
import tys.frontier.code.expression.FExpression;
import tys.frontier.code.statement.FStatement;

public class FForEach implements FLoop {

    FVariable iterator;
    FExpression container;
    FStatement body;

}
