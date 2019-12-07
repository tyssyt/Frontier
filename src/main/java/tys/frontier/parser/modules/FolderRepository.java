package tys.frontier.parser.modules;

import tys.frontier.style.Style;
import tys.frontier.util.Utils;

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
    public Path resolve(String name) {
        Path resolve = folder.resolve(name + Utils.filesep + name + ".front");
        if (Files.isRegularFile(resolve))
            return resolve;
        else
            return null;
    }

    @Override
    public Style getStyle() {
        return style;
    }
}
