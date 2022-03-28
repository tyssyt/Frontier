package tys.frontier.logging;

import java.io.PrintStream;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Formatter;

public final class Logger {

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss.SSS");

    public enum Level {
        FATAL,
        ERROR,
        WARNING,
        INFO,
        DEBUG;

        public PrintStream logTarget() {
            return switch (this) {
                case FATAL, ERROR -> System.err;
                case WARNING, INFO, DEBUG -> System.out;
            };
        }

        public boolean canLogTo(Level other) {
            return this.ordinal() >= other.ordinal();
        }
    }

    private static volatile Level level = Level.INFO;
    public static Level getLevel() {
        return level;
    }
    public static void setLevel(Level newLevel) {
        level = newLevel;
    }

    public static void fatal(String message) {
        writeLog(Level.FATAL, message, null, null);
    }
    public static void fatal(String message, Object... args) {
        writeLog(Level.FATAL, message, null, args);
    }
    public static void fatal(String message, Throwable cause) {
        writeLog(Level.FATAL, message, cause, null);
    }
    public static void fatal(String message, Throwable cause, Object... args) {
        writeLog(Level.FATAL, message, cause, args);
    }

    public static void error(String message) {
        writeLog(Level.ERROR, message, null, null);
    }
    public static void error(String message, Object... args) {
        writeLog(Level.ERROR, message, null, args);
    }
    public static void error(String message, Throwable cause) {
        writeLog(Level.ERROR, message, cause, null);
    }
    public static void error(String message, Throwable cause, Object... args) {
        writeLog(Level.ERROR, message, cause, args);
    }

    public static void warn(String message) {
        writeLog(Level.WARNING, message, null, null);
    }
    public static void warn(String message, Object... args) {
        writeLog(Level.WARNING, message, null, args);
    }
    public static void warn(String message, Throwable cause) {
        writeLog(Level.WARNING, message, cause, null);
    }
    public static void warn(String message, Throwable cause, Object... args) {
        writeLog(Level.WARNING, message, cause, args);
    }

    public static void info(String message) {
        writeLog(Level.INFO, message, null, null);
    }
    public static void info(String message, Object... args) {
        writeLog(Level.INFO, message, null, args);
    }
    public static void info(String message, Throwable cause) {
        writeLog(Level.INFO, message, cause, null);
    }
    public static void info(String message, Throwable cause, Object... args) {
        writeLog(Level.INFO, message, cause, args);
    }

    public static void debug(String message) {
        writeLog(Level.DEBUG, message, null, null);
    }
    public static void debug(String message, Object... args) {
        writeLog(Level.DEBUG, message, null, args);
    }
    public static void debug(String message, Throwable cause) {
        writeLog(Level.DEBUG, message, cause, null);
    }
    public static void debug(String message, Throwable cause, Object... args) {
        writeLog(Level.DEBUG, message, cause, args);
    }


    private static void writeLog(Level level, String message, Throwable cause, Object[] args) {
        if (!level.canLogTo(getLevel()))
            return;

        StackTraceElement stackTraceElement = Thread.currentThread().getStackTrace()[3];
        String fullClassName = stackTraceElement.getClassName();
        String className = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);

        StringBuilder sb = new StringBuilder()
                .append(level).append(' ')
                .append(LocalTime.now().format(DATE_TIME_FORMATTER))
                .append(" [").append(Thread.currentThread().getName()).append("] ")
                .append(className).append('.').append(stackTraceElement.getMethodName())
                .append('(').append(stackTraceElement.getLineNumber()).append("): ");

        if (args == null)
            sb.append(message);
        else
            new Formatter(sb).format(message, args);

        if (cause != null)
            sb.append(", caused by: ").append(cause);

        level.logTarget().println(sb);
    }

    private Logger() {}
}
