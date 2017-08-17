package tys.frontier.logging;

public interface Logger {

    void log (Object source, String message, Object cause, Level severity);

    //for convenience
    default void log (Object source, String message, Level severity) {
        log(source, message, null, severity);
    }

    default void debug (Object source, String message, Object cause) {
        log(source, message, cause, Level.DEBUG);
    }

    default void debug (Object source, String message) {
        debug(source, message, null);
    }

    default void info (Object source, String message, Object cause) {
        log(source, message, cause, Level.INFO);
    }

    default void info (Object source, String message) {
        info(source, message, null);
    }

    default void warning (Object source, String message, Object cause) {
        log(source, message, cause, Level.WARNING);
    }

    default void warning (Object source, String message) {
        warning(source, message, null);
    }

    default void warning (Object source, Throwable cause) {
        warning(source, null, cause);
    }

    default void error (Object source, String message, Object cause) {
        log(source, message, cause, Level.ERROR);
    }

    default void error (Object source, String message) {
        error(source, message, null);
    }

    default void error (Object source, Throwable cause) {
        error(source, null, cause);
    }

    default void fatal (Object source, String message, Object cause) {
        log(source, message, cause, Level.FATAL);
    }

    default void fatal (Object source, String message) {
        fatal(source, message, null);
    }

    default void fatal (Object source, Throwable cause) {
        fatal(source, null, cause);
    }
    enum Level{
        FATAL,
        ERROR,
        WARNING,
        INFO,
        DEBUG
    }
}
