package tys.frontier.parser;

import com.google.common.collect.Iterables;
import org.junit.Before;
import org.junit.Test;
import tys.frontier.State;
import tys.frontier.code.FField;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.logging.Log;
import tys.frontier.logging.Logger;
import tys.frontier.logging.StdOutLogger;
import tys.frontier.parser.modules.ImportResolver;
import tys.frontier.parser.modules.ResourceRepository;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.style.Style;
import tys.frontier.util.FileUtils;

import java.io.IOException;
import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ParserTest {

    private static final String prefix = "Parser" + FileUtils.filesep + "SyntaxErrors" + FileUtils.filesep;

    @Before
    public void setUp() {
        Logger logger = Log.DEFAULT_LOGGER;
        if (logger instanceof StdOutLogger)
            ((StdOutLogger) logger).setLevel(Logger.Level.WARNING);
        State.reset();
        State.get().setImportResolver(new ImportResolver(Collections.singletonList(ResourceRepository.INSTANCE)));
    }

    private SyntaxError parseSyntaxError(String file) throws IOException {
        try {
            Parser.parse(FileUtils.pathToResource(prefix + file), Style.DEFAULT_STYLE);
        } catch (SyntaxErrors es) {
            return Iterables.getOnlyElement(es.errors);
        } catch (SyntaxError syntaxError) {
            return syntaxError;
        }
        throw new RuntimeException("no error");
    }

    @Test
    public void parseAccessForbidden() throws Exception {
        SyntaxError e = parseSyntaxError("AccessForbidden.front");
        assertEquals(AccessForbidden.class, e.getClass());
    }
    @Test
    public void parseAccessForbiddenClass() throws Exception {
        SyntaxError e = parseSyntaxError("AccessForbiddenClass.front");
        assertEquals(UndeclaredVariable.class, e.getClass()); //this was TypeNotFound before the identifier change, due to how findLocal findType works we now classify B as not found local variable and not found type
    }
    @Test
    public void parseAvoidCastingEverythingToBool() throws Exception {
        SyntaxError e = parseSyntaxError("AvoidCastingEverythingToBool.front");
        assertEquals(IncompatibleTypes.class, e.getClass());
    }
    @Test
    public void parseBreakOutsideLoop() throws Exception {
        SyntaxError e = parseSyntaxError("BreakOutsideLoop.front");
        assertEquals(StatementOutsideLoop.class, e.getClass());
    }
    @Test
    public void parseClassIdentifierCollision() throws Exception {
        SyntaxError e = parseSyntaxError("ClassIdentifierCollision.front");
        assertEquals(IdentifierCollision.class, e.getClass());
        IdentifierCollision c = ((IdentifierCollision) e);
        assertTrue(c.a instanceof FClass);
        assertTrue(c.b instanceof DefaultNamespace);
    }
    @Test
    public void parseContinueOutsideLoop() throws Exception {
        SyntaxError e = parseSyntaxError("ContinueOutsideLoop.front");
        assertEquals(StatementOutsideLoop.class, e.getClass());
    }
    @Test
    public void parseCyclicDelegate() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicDelegate.front");
        assertEquals(CyclicDelegate.class, e.getClass());
    }
    @Test
    public void parseCyclicDelegate2() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicDelegate2.front");
        assertEquals(CyclicDelegate.class, e.getClass());
    }
    @Test
    public void parseCyclicInclude() throws Exception {
        SyntaxError e = parseSyntaxError("CyclicInclude.front");
        assertEquals(CyclicInclude.class, e.getClass());
    }
    @Test
    public void parseDelegateFromStaticField() throws Exception {
        SyntaxError e = parseSyntaxError("DelegateFromStaticField.front");
        assertEquals(DelegateFromStaticField.class, e.getClass());
    }
    @Test
    public void parseDelegateFromTypeVar() throws Exception {
        SyntaxError e = parseSyntaxError("DelegateFromTypeVar.front");
        assertEquals(DelegateFromTypeVar.class, e.getClass());
    }
    @Test
    public void parseDynamicCallWithKeywordArgs() throws Exception {
        SyntaxError e = parseSyntaxError("DynamicCallWithKeywordArgs.front");
        assertEquals(DynamicCallWithKeywordArgs.class, e.getClass());
    }
    @Test
    public void parseFieldIdentifierCollision() throws Exception {
        SyntaxError e = parseSyntaxError("FieldIdentifierCollision.front");
        assertEquals(IdentifierCollision.class, e.getClass());
        IdentifierCollision c = ((IdentifierCollision) e);
        assertTrue(c.a instanceof FField);
        assertTrue(c.b instanceof FField);
    }
    @Test
    public void parseFieldNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("FieldNotFound.front");
        assertEquals(FunctionNotFound.class, e.getClass());
    }
    @Test
    public void parseFunctionNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("FunctionNotFound.front");
        assertEquals(FunctionNotFound.class, e.getClass());
    }
    @Test
    public void parseIncompatibleTypesAssignment() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesAssignment.front");
        assertEquals(IncompatibleTypes.class, e.getClass());
    }
    @Test
    public void parseIncompatibleTypesIf() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesIf.front");
        assertEquals(IncompatibleTypes.class, e.getClass());
    }
    @Test
    public void parseIncompatibleTypesReturn() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesReturn.front");
        assertEquals(IncompatibleTypes.class, e.getClass());
    }
    @Test
    public void parseIncompatibleTypesWhile() throws Exception {
        SyntaxError e = parseSyntaxError("IncompatibleTypesWhile.front");
        assertEquals(IncompatibleTypes.class, e.getClass());
    }
    @Test
    public void parseInvalidForDeclaration() throws Exception {
        SyntaxError e = parseSyntaxError("InvalidForDeclaration.front");
        assertEquals(InvalidForDeclaration.class, e.getClass());
    }
    @Test
    public void parseInvalidOpenDeclaration1() throws Exception {
        SyntaxError e = parseSyntaxError("InvalidOpenDeclaration1.front");
        assertEquals(InvalidOpenDeclaration.class, e.getClass());
    }
    @Test
    public void parseInvalidOpenDeclaration2() throws Exception {
        SyntaxError e = parseSyntaxError("InvalidOpenDeclaration2.front");
        assertEquals(InvalidOpenDeclaration.class, e.getClass());
    }
    @Test
    public void parseInvalidSignatureRemoteFunctionDeclaration() throws Exception {
        SyntaxError e = parseSyntaxError("InvalidSignatureRemoteFunctionDeclaration.front");
        assertEquals(InvalidSignatureOpenOverload.class, e.getClass());
    }
    @Test
    public void parseMissingReturn() throws Exception {
        SyntaxError e = parseSyntaxError("MissingReturn.front");
        assertEquals(MissingReturn.class, e.getClass());
        assertEquals("haveFun2", ((MissingReturn) e).function.getIdentifier().name);
    }
    @Test
    public void parseNativeWithBody() throws Exception {
        SyntaxError e = parseSyntaxError("NativeWithBody.front");
        assertEquals(NativeWithBody.class, e.getClass());
    }
    @Test
    public void parseNonOpenRemoteFunctionDeclaration() throws Exception {
        SyntaxError e = parseSyntaxError("NonOpenRemoteFunctionDeclaration.front");
        assertEquals(NonOpenRemoteFunctionDeclaration.class, e.getClass());
    }
    @Test
    public void parseNonOptionalExMark() throws Exception {
        SyntaxError e = parseSyntaxError("NonOptionalExMark.front");
        assertEquals(NonOptionalExMark.class, e.getClass());
    }
    @Test
    public void parseNotEnoughArgumentsAssign() throws Exception {
        SyntaxError e = parseSyntaxError("NotEnoughArgumentsAssign.front");
        assertEquals(NotEnoughArguments.class, e.getClass());
    }
    @Test
    public void parseNotEnoughArgumentsReturn() throws Exception {
        SyntaxError e = parseSyntaxError("NotEnoughArgumentsReturn.front");
        assertEquals(NotEnoughArguments.class, e.getClass());
    }
    @Test
    public void parseParameterizedTypeVariable() throws Exception {
        SyntaxError e = parseSyntaxError("ParameterizedTypeVariable.front");
        assertEquals(ParameterizedTypeVariable.class, e.getClass());
    }
    @Test
    public void parseSignatureCollision() throws Exception {
        SyntaxError e = parseSyntaxError("SignatureCollision.front");
        assertEquals(SignatureCollision.class, e.getClass());
    }
    @Test
    public void parseTooManyArgumentsAssign() throws Exception {
        SyntaxError e = parseSyntaxError("TooManyArgumentsAssign.front");
        assertEquals(TooManyArguments.class, e.getClass());
    }
    @Test
    public void parseTooManyArgumentsReturn() throws Exception {
        SyntaxError e = parseSyntaxError("TooManyArgumentsReturn.front");
        assertEquals(TooManyArguments.class, e.getClass());
    }
    @Test
    public void parseTwiceDefinedLocalVariable() throws Exception {
        SyntaxError e = parseSyntaxError("TwiceDefinedLocalVariable.front");
        assertEquals(TwiceDefinedLocalVariable.class, e.getClass());
    }
    @Test
    public void parseTwiceDefinedLocalVariableFor() throws Exception {
        SyntaxError e = parseSyntaxError("TwiceDefinedLocalVariableFor.front");
        assertEquals(TwiceDefinedLocalVariable.class, e.getClass());
    }
    @Test
    public void parseTypeDoesNotImplementFor() throws Exception {
        SyntaxError e = parseSyntaxError("TypeDoesNotImplementFor.front");
        assertEquals(TypeDoesNotImplementFor.class, e.getClass());
    }
    @Test
    public void parseTypeNotFound() throws Exception {
        SyntaxError e = parseSyntaxError("TypeNotFound.front");
        assertEquals(TypeNotFound.class, e.getClass());
    }
    @Test
    public void parseUndeclaredVariable() throws Exception {
        SyntaxError e = parseSyntaxError("UndeclaredVariable.front");
        assertEquals(UndeclaredVariable.class, e.getClass());
    }
    @Test
    public void parseUndeclaredVariableWhere() throws Exception {
        SyntaxError e = parseSyntaxError("UndeclaredVariableWhere.front");
        assertEquals(UndeclaredVariable.class, e.getClass());
        assertEquals("S", ((UndeclaredVariable)e).identifier.name);
    }
    @Test
    public void parseUnfulfillableConstraintFixed() throws Exception {
        SyntaxError e = parseSyntaxError("UnfulfillableConstraintFixed.front");
        assertEquals(FunctionNotFound.class, e.getClass());
    }
    @Test
    public void parseUnfulfillableConstraint() throws Exception {
        SyntaxError e = parseSyntaxError("UnfulfillableConstraints.front");
        assertEquals(UnfulfillableConstraints.class, e.getClass());
    }
    @Test
    public void parseUntypedVariable() throws Exception {
        SyntaxError e = parseSyntaxError("UntypedVariable.front");
        assertEquals(UntypedVariable.class, e.getClass());
    }
    @Test
    public void parseUntypedFunctionAddress() throws Exception {
        SyntaxError e = parseSyntaxError("UntypedFunctionAddress.front");
        assertEquals(UnfulfillableConstraints.class, e.getClass());
    }
    @Test
    public void WrongNumberOfIdentifiersInFor() throws Exception {
        SyntaxError e = parseSyntaxError("WrongNumberOfIdentifiersInFor.front");
        assertEquals(WrongNumberOfIdentifiersInFor.class, e.getClass());
    }
    @Test
    public void parseWrongNumberOfTypeArguments() throws Exception {
        SyntaxError e = parseSyntaxError("WrongNumberOfTypeArguments.front");
        assertEquals(WrongNumberOfTypeArguments.class, e.getClass());
    }
    @Test
    public void parseWrongNumberOfTypeArguments2() throws Exception {
        SyntaxError e = parseSyntaxError("WrongNumberOfTypeArguments2.front");
        assertEquals(WrongNumberOfTypeArguments.class, e.getClass());
    }
}