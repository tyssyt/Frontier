package tys.frontier.parser.modules;

import com.google.common.collect.ImmutableMap;
import tys.frontier.State;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.module.Module;
import tys.frontier.code.module.SimpleModule;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.predefinedClasses.FFieldType;
import tys.frontier.code.predefinedClasses.FTypeType;
import tys.frontier.parser.Parser;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;
import tys.frontier.util.FileUtils;

import java.io.IOException;
import java.nio.file.Path;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

public class ResourceRepository implements ModuleRepository {

    public static final ResourceRepository INSTANCE = new ResourceRepository();

    private ResourceRepository() {
    }

    @Override
    public Module resolve(String name) throws SyntaxErrors, IOException, SyntaxError { //TODO make this work when running in a Jar
        switch (name) {
            case "Type":
                return createTypeModule();
            default:
                Path path = FileUtils.pathToResource("Frontier Libs/" + name + ".front");
                return Parser.parse(path, getStyle());
        }
    }

    @Override
    public Style getStyle() {
        return Style.DEFAULT_STYLE;
    }

    private static SimpleModule createTypeModule() throws SyntaxErrors, SyntaxError {
        Module _import = State.get().getImportResolver().requestModule("Strings"); //TODO requests strings specifically from this repo...
        ImmutableMap<FIdentifier, DefaultNamespace> namespaces = ImmutableMap.of(FTypeType.IDENTIFIER, FTypeType.INSTANCE.getNamespace(), FFieldType.IDENTIFIER, FFieldType.INSTANCE.getNamespace());
        SimpleModule res = new SimpleModule(namespaces, singletonList(_import), emptyList());
        State.get().setTypeModule(res);
        return res;
    }
}
