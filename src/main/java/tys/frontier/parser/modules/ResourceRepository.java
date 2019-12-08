package tys.frontier.parser.modules;

import tys.frontier.style.Style;
import tys.frontier.util.FileUtils;

import java.nio.file.Path;

public class ResourceRepository implements ModuleRepository {

    public static final ResourceRepository INSTANCE = new ResourceRepository();

    private ResourceRepository() {
    }

    @Override
    public Path resolve(String name) { //TODO make this work when running in a Jar
        return FileUtils.pathToResource("Frontier Libs/" + name + ".front");
    }

    @Override
    public Style getStyle() {
        return Style.DEFAULT_STYLE;
    }
}
