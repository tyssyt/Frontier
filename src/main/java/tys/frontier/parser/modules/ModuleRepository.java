package tys.frontier.parser.modules;

import tys.frontier.style.Style;

import java.nio.file.Path;

public interface ModuleRepository {

    Path resolve(String name);

    Style getStyle();

}
