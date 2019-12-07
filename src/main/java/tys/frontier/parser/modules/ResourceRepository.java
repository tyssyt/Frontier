package tys.frontier.parser.modules;

import tys.frontier.style.Style;

import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ResourceRepository implements ModuleRepository {

    public static final ResourceRepository INSTANCE = new ResourceRepository();

    private ResourceRepository() {
    }

    @Override
    public Path resolve(String name) { //TODO make this work when running in a Jar
        try {
            URL resource = this.getClass().getResource("/Frontier Libs/" + name + ".front");
            if (resource == null)
                return null;
            return Paths.get(resource.toURI());
        } catch (URISyntaxException e) {
            return null;
        }
    }

    @Override
    public Style getStyle() {
        return Style.DEFAULT_STYLE;
    }
}
