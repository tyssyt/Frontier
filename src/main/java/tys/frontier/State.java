package tys.frontier;

import tys.frontier.code.module.Module;
import tys.frontier.parser.Parser;
import tys.frontier.parser.modules.ImportResolver;

import java.io.File;

public class State {

    private static State instance = new State();

    public static State get() {
        return instance;
    }

    private Parser currentParser = null;
    private ImportResolver importResolver;
    private File tempDir;

    private Module typeModule;

    public Parser getCurrentParser() {
        return currentParser;
    }

    public Parser setCurrentParser(Parser currentParser) {
        Parser old = this.currentParser;
        this.currentParser = currentParser;
        return old;
    }

    public ImportResolver getImportResolver() {
        return importResolver;
    }

    public void setImportResolver(ImportResolver importResolver) {
        this.importResolver = importResolver;
    }

    public File getTempDir() {
        return tempDir;
    }

    public void setTempDir(File tempDir) {
        assert this.tempDir == null || tempDir == null;
        this.tempDir = tempDir;
    }

    public Module getTypeModule() {
        return typeModule;
    }

    public void setTypeModule(Module typeModule) {
        assert this.typeModule == null;
        this.typeModule = typeModule;
    }
}
