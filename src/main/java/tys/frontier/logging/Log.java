package tys.frontier.logging;

import tys.frontier.logging.Logger.Level;

public class Log {

    public static Logger DEFAULT_LOGGER = new StdOutLogger();

    public static void log (Object source, String message, Object cause, Level severity) {
        DEFAULT_LOGGER.log(source, message, cause, severity);
    }
    public static void log (Object source, String message, Level severity) {
        DEFAULT_LOGGER.log(source, message, severity);
    }

    public static void debug (Object source, String message, Object cause) {
        DEFAULT_LOGGER.debug(source, message, cause);
    }
    public static void debug (Object source, String message) {
        DEFAULT_LOGGER.debug(source, message);
    }

    public static void info (Object source, String message, Object cause) {
        DEFAULT_LOGGER.info(source, message, cause);
    }
    public static void info (Object source, String message) {
        DEFAULT_LOGGER.info(source, message);
    }

    public static void warning (Object source, String message, Object cause) {
        DEFAULT_LOGGER.warning(source, message, cause);
    }
    public static void warning (Object source, String message) {
        DEFAULT_LOGGER.warning(source, message);
    }
    public static void warning (Object source, Throwable cause) {
        DEFAULT_LOGGER.warning(source, cause);
    }

    public static void error (Object source, String message, Object cause) {
        DEFAULT_LOGGER.error(source, message, cause);
    }
    public static void error (Object source, String message) {
        DEFAULT_LOGGER.error(source, message);
    }
    public static void error (Object source, Throwable cause) {
        DEFAULT_LOGGER.error(source, cause);
    }

    public static void fatal (Object source, String message, Object cause) {
        DEFAULT_LOGGER.fatal(source, message, cause);
    }
    public static void fatal (Object source, String message) {
        DEFAULT_LOGGER.fatal(source, message);
    }
    public static void fatal (Object source, Throwable cause) {
        DEFAULT_LOGGER.fatal(source, cause);
    }

}
