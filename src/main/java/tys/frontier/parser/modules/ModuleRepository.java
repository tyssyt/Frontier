package tys.frontier.parser.modules;

import tys.frontier.code.module.Module;
import tys.frontier.parser.syntaxErrors.SyntaxError;
import tys.frontier.parser.syntaxErrors.SyntaxErrors;
import tys.frontier.style.Style;

import java.io.IOException;

public interface ModuleRepository {

    Module resolve(String name) throws SyntaxErrors, IOException, SyntaxError;

    Style getStyle();

}
