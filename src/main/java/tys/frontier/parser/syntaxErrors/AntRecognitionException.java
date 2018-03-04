package tys.frontier.parser.syntaxErrors;

import org.antlr.v4.runtime.RecognitionException;

public class AntRecognitionException extends SyntaxError {

    public AntRecognitionException(RecognitionException cause) {
        super(cause);
    }

}
