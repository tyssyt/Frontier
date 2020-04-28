package tys.frontier.parser.modules;

import tys.frontier.code.module.Module;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FolderRepository implements ModuleRepository {

    private Path folder;
    private Style style;

    public FolderRepository(Path folder, Style style) {
        this.folder = folder;
        this.style = style;
    }

    @Override
    public Module resolve(String name) throws SyntaxErrors, IOException, SyntaxError {
        Path resolve = folder.resolve(name + Utils.filesep + name + ".front");
        if (Files.isRegularFile(resolve))
            return Parser.parse(resolve, getStyle());
        else
            return null;
    }

    @Override
    public Style getStyle() {
        return style;
    }
}
