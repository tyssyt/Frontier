package tys.frontier.logging;

import java.io.PrintStream;
import java.util.Date;

import static tys.frontier.logging.Logger.Level.INFO;

public class StdOutLogger implements Logger {

    private Level level = INFO;

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

    @Override
    public void log(Object source, String message, Object cause, Level severity) {
        Date now = new Date();
        if (severity.ordinal() > level.ordinal())
            return;
        PrintStream out;
        switch (severity) {
            case WARNING: case ERROR: case FATAL:
                out = System.err;
                break;
            default:
                out = System.out;
        }
        StringBuilder sb = new StringBuilder()
                .append(now).append(", " ).append(source).append(": ").append(message);
        if (cause != null)
            sb.append(", caused by: ").append(cause);
        out.println(sb);
    }
}
