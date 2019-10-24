package tys;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.log4j.spi.ThrowableInformation;
import org.slf4j.MDC;

public class UnitTestLogAppender extends AppenderSkeleton implements AutoCloseable {
	private final StringBuffer logBuffer = new StringBuffer();
	private final List<ThrowableInformation> loggedThrowableInformation = new ArrayList<>();
	private Logger logger;


	public UnitTestLogAppender() {
		logger = Logger.getRootLogger();
		logger.addAppender(this);
	}

	@Override
	public boolean requiresLayout() {
		return false;
	}

	@Override
	protected void append(final LoggingEvent loggingEvent) {
		logBuffer.append(loggingEvent.getMessage());
		logBuffer.append("\n");
		if (loggingEvent.getThrowableInformation() != null) {
			loggedThrowableInformation.add(loggingEvent.getThrowableInformation());
			loggingEvent.getMDCCopy();
			System.out.println("MDC: " + MDC.getCopyOfContextMap());
		}
	}

	@Override
	public void close() {
		logger.removeAppender(this);
	}

	public String getLogMessages() {
		return logBuffer.toString();
	}

	public List<ThrowableInformation> getLoggedThrowableInformation() {
		return loggedThrowableInformation;
	}
}
