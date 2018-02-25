package tys.frontier.backend.frontier;

import tys.frontier.backend.Backend;
import tys.frontier.backend.frontier.visitors.ToFrontierFileVisitor;
import tys.frontier.code.FFile;
import tys.frontier.style.Style;

public class FrontierBackend implements Backend {


    public static void translate(FFile file, Style style, Appendable out) {
        ToFrontierFileVisitor.create(style, out).enterFile(file);
    }




}
