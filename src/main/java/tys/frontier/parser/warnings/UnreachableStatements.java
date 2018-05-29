package tys.frontier.parser.warnings;

import tys.frontier.code.statement.FStatement;

import java.util.List;

public class UnreachableStatements extends Warning {

    final List<FStatement> unreachableStatements;

    public UnreachableStatements(List<FStatement> unreachableStatements) {
        this.unreachableStatements = unreachableStatements;
    }
}
