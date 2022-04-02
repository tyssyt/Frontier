package tys.frontier.parser;

import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.syntaxTree.SyntaxTreeData;

import java.nio.file.Path;

public final class WipFile {

    public final Path path;
    public final SyntaxTreeData treeData;

    public WipFile(Path path, FrontierParser.FileContext fileContext) {
        this.path = path;
        this.treeData = new SyntaxTreeData(fileContext);
    }
}
