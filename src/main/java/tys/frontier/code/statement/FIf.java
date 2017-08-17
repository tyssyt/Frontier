package tys.frontier.code.statement;

import tys.frontier.code.expression.FExpression;

public class FIf implements FStatement {

    FExpression condition;
    FStatement then;
    FStatement elze; //Optional

}
