package tys.frontier.logging;

import java.io.PrintStream;
import java.util.Date;

public class StdOutLogger implements Logger {

    @Override
    public void log(Object source, String message, Object cause, Level severity) {
        Date now = new Date();
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
