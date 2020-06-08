package tys.frontier.code.module;

import java.nio.file.Path;

public class Include {

    public final Path path;
    public final boolean out;

    public Include(Path path, boolean out) {
        this.path = path;
        this.out = out;
    }
}
