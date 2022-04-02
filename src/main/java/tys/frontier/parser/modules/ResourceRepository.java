package tys.frontier.parser.modules;

import tys.frontier.State;
import tys.frontier.code.module.FrontierModule;
import tys.frontier.code.module.Module;
import tys.frontier.code.predefinedClasses.FFieldType;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.FileUtils;

import java.io.IOException;
import java.util.List;

public class ResourceRepository implements ModuleRepository {

    public static final ResourceRepository INSTANCE = new ResourceRepository();

    private ResourceRepository() {
    }

    @Override
    public Module resolve(String name) throws SyntaxErrors, IOException, SyntaxError { //TODO make this work when running in a Jar
        return switch (name) {
            case "Type" -> createTypeModule();
            default -> Parser.parse(FileUtils.pathToResource("Frontier Libs/" + name + ".front"), getStyle());
        };
    }

    @Override
    public Style getStyle() {
        return Style.DEFAULT_STYLE;
    }

    private static Module createTypeModule() throws SyntaxErrors, SyntaxError {
        Module _import = State.get().getImportResolver().requestModule("Strings"); //TODO requests strings specifically from this repo...
        FrontierModule typeModule = new FrontierModule("Type", List.of(_import), List.of(), List.of(), List.of(FTypeType.INSTANCE.getNamespace(), FFieldType.INSTANCE.getNamespace()));
        State.get().setTypeModule(typeModule);
        return typeModule;
    }
}
