package tys.frontier.backend.llvm;

import com.google.common.collect.Iterables;
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.FArray;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.ClassWalker;
import tys.frontier.modules.io.IOClass;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.collect.Iterables.getOnlyElement;
import static org.bytedeco.javacpp.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;

class LLVMTransformer implements
        AutoCloseable,
        ClassWalker<LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef> {

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    private LLVMModule module;
    private LLVMBuilderRef builder;
    private LLVMBuilderRef entryBlockAllocaBuilder;
    private Map<FField, LLVMValueRef> fields = new HashMap<>(); //TODO why this not used?
    private Map<FLocalVariable, LLVMValueRef> localVars = new HashMap<>();

    public LLVMTransformer(LLVMModule module) {
        this.module = module;
        this.builder = module.createBuilder();
        this.entryBlockAllocaBuilder = module.createBuilder();
    }

    @Override
    public void close() {
        LLVMDisposeBuilder(builder);
        LLVMDisposeBuilder(entryBlockAllocaBuilder);
    }

    private LLVMValueRef getCurrentFunction() {
        return LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    }

    private LLVMValueRef createEntryBlockAlloca(FLocalVariable variable) {
        LLVMValueRef res = LLVMBuildAlloca(entryBlockAllocaBuilder, module.getLlvmType(variable.getType()), variable.getIdentifier().name);
        LLVMValueRef old = localVars.put(variable, res);
        if (old != null)
            Utils.cantHappen();
        return res;
    }

    @Override
    public LLVMValueRef visitFunction(FFunction function) {
        LLVMValueRef res = LLVMGetNamedFunction(module.getModule(), getFunctionName(function));
        if (res.isNull()) {
            Utils.cantHappen();
        }

        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(res, "alloca");
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(res, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);

        //fill in parameters
        int offset = 0;
        if (!function.isStatic()) {
            LLVMValueRef alloca = createEntryBlockAlloca(function.getClazz().getThis());
            LLVMBuildStore(builder, LLVMGetParam(res, 0), alloca);
            offset++;
        }
        List<FParameter> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++) {
            LLVMValueRef alloca = createEntryBlockAlloca(fParams.get(i));
            LLVMBuildStore(builder, LLVMGetParam(res, i+offset), alloca);
        }

        //do the body
        for (FStatement statement : function.getBody())
            statement.accept(this);

        //finish
        LLVMBuildBr(entryBlockAllocaBuilder, entryBlock);
        if (function.getType() == FVoid.INSTANCE)
            LLVMBuildRetVoid(builder);
        localVars.clear();
        //LLVMViewFunctionCFG(res);
        return res;
    }

    @Override
    public LLVMValueRef visitBlock(FBlock block) {
        LLVMValueRef last = null;
        for (FStatement statement : block.getStatements())
            last = statement.accept(this);
        return last;
    }

    @Override
    public LLVMValueRef visitExpressionStatement(FExpressionStatement statement) {
        return statement.getExpression().accept(this);
    }

    @Override
    public LLVMValueRef visitIf(FIf fIf) {
        //TODO some analysis if blocks have flow control at the end (aka whether we need the branch) would be nice
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef ifBlock = LLVMAppendBasicBlock(currentFunction, "if");
        LLVMBasicBlockRef thenBlock = LLVMAppendBasicBlock(currentFunction, "then");
        LLVMBasicBlockRef continueBlock = LLVMAppendBasicBlock(currentFunction, "after_if");
        LLVMBasicBlockRef elseBlock = fIf.getElse().isPresent() ? LLVMInsertBasicBlock(continueBlock, "else") : continueBlock;

        LLVMBuildBr(builder, ifBlock);

        LLVMPositionBuilderAtEnd(builder, ifBlock);
        LLVMValueRef condition = fIf.getCondition().accept(this);
        LLVMBuildCondBr(builder, condition, thenBlock, elseBlock);

        LLVMPositionBuilderAtEnd(builder, thenBlock);
        fIf.getThen().accept(this);
        LLVMBuildBr(builder, continueBlock);

        fIf.getElse().ifPresent(elze -> {
            LLVMPositionBuilderAtEnd(builder, elseBlock);
            elze.accept(this);
            LLVMBuildBr(builder, continueBlock);
        });

        LLVMPositionBuilderAtEnd(builder, continueBlock);
        return null;
    }

    @Override
    public LLVMValueRef visitReturn(FReturn fReturn) {
        return fReturn.getExpression()
                .map(expression -> LLVMBuildRet(builder, expression.accept(this)))
                .orElseGet(() -> LLVMBuildRetVoid(builder));
    }

    @Override
    public LLVMValueRef visitVarDeclaration(FVarDeclaration declaration) {
        LLVMValueRef alloc = createEntryBlockAlloca(declaration.getVar());
        return declaration.getAssignment().map(assignment -> assignment.accept(this)).orElse(alloc);
    }

    @Override
    public LLVMValueRef visitVarAssignment(FVarAssignment assignment) {
        if (assignment.getOperator() != FVarAssignment.Operator.ASSIGN) {
            Utils.NYI("Operator " + assignment.getOperator());
        }
        LLVMValueRef value = assignment.getValue().accept(this);
        LLVMValueRef variableAddress = assignment.getVariableExpression().accept(this);
        return LLVMBuildStore(builder, value, variableAddress);
    }

    @Override
    public LLVMValueRef visitWhile(FWhile fWhile) {
        return Utils.NYI("while");
    }

    @Override
    public LLVMValueRef visitFor(FFor fFor) {
        return Utils.NYI("for");
    }

    @Override
    public LLVMValueRef visitForEach(FForEach forEach) {
        return Utils.NYI("for each");
    }

    @Override
    public LLVMValueRef visitEmpty(FEmptyStatement statement) {
        return null;
    }

    @Override
    public LLVMValueRef visitBreak(FBreak fBreak) {
        return Utils.NYI("break");
    }

    @Override
    public LLVMValueRef visitContinue(FContinue fContinue) {
        return Utils.NYI("continue");
    }

    @Override
    public LLVMValueRef visitArrayAccess(FArrayAccess arrayAccess) {
        LLVMValueRef array = arrayAccess.getArray().accept(this);
        LLVMValueRef index = arrayAccess.getIndex().accept(this);
        PointerPointer<LLVMValueRef> indices = new PointerPointer<>(
                LLVMConstInt(module.getLlvmType(FIntN._32), 0, FALSE),
                LLVMConstInt(module.getLlvmType(FIntN._32), 1, FALSE),
                index
        );
        LLVMValueRef address = LLVMBuildGEP(builder, array, indices, 3, "GEP_array");

        switch (arrayAccess.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, "load_array");
            case STORE:
                return address;
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitBrackets(FBracketsExpression brackets) {
        return brackets.getInner().accept(this);
    }

    @Override
    public LLVMValueRef visitImplicitCast(FImplicitCast implicitCast) {
        LLVMValueRef toCast = implicitCast.getCastedExpression().accept(this);
        LLVMTypeRef targetType = module.getLlvmType(implicitCast.getType());
        switch (implicitCast.getCastType()) {
            case INTEGER_PROMOTION:
                return LLVMBuildSExt(builder, toCast, targetType, "sExt");
            case FLOAT_PROMOTION:
                return LLVMBuildFPExt(builder, toCast, targetType, "fpExt");
            case INT_TO_FLOAT:
                return LLVMBuildSIToFP(builder, toCast, targetType, "siToFP");
            case OBJECT_DEMOTION:
                return Utils.NYI("object demotion");
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitExplicitCast(FExplicitCast explicitCast) {
        LLVMValueRef toCast = explicitCast.getCastedExpression().accept(this);
        LLVMTypeRef targetType = module.getLlvmType(explicitCast.getType());
        switch (explicitCast.getCastType()) {
            case INTEGER_DEMOTION:
                return LLVMBuildTrunc(builder, toCast, targetType, "trunc");
            case FLOAT_DEMOTION:
                return LLVMBuildFPTrunc(builder, toCast, targetType, "fpTrunc");
            case FLOAT_TO_INT:
                return LLVMBuildFPToSI(builder, toCast, targetType, "fpToSI");
            case OBJECT_PROMOTION:
                return Utils.NYI("object promotion");
            default:
                return Utils.cantHappen();
        }
    }

    private LLVMValueRef predefinedUnary(FFunctionCall functionCall) {
        LLVMValueRef arg;
        if (functionCall.getFunction().isStatic())
            arg = functionCall.getArguments().get(0).accept(this);
        else
            arg = functionCall.getObject().accept(this);

        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        if (id.equals(FUnaryOperator.Pre.NOT.identifier))
            return LLVMBuildNot(builder, arg, "not");
        else if (id.equals(FUnaryOperator.Pre.NEG.identifier))
            return LLVMBuildNeg(builder, arg, "neg");
        else
            return Utils.cantHappen();
    }

    private LLVMValueRef predefinedBinary(FFunctionCall functionCall) {
        LLVMValueRef left;
        LLVMValueRef right;
        if (functionCall.getFunction().isStatic()) {
            left = functionCall.getArguments().get(0).accept(this);
            right = functionCall.getArguments().get(1).accept(this);
        } else {
            left = functionCall.getObject().accept(this);
            right = functionCall.getArguments().get(0).accept(this);
        }

        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        if (id.equals(FBinaryOperator.Arith.PLUS.identifier)) {
            return LLVMBuildAdd(builder, left, right, "add");
        } else if (id.equals(FBinaryOperator.Arith.MINUS.identifier)) {
            return LLVMBuildSub(builder, left, right, "sub");
        } else if (id.equals(FBinaryOperator.Arith.TIMES.identifier)) {
            return LLVMBuildMul(builder, left, right, "mul");
        } else if (id.equals(FBinaryOperator.Arith.DIVIDED.identifier)) {
            return LLVMBuildSDiv(builder, left, right, "sdiv");
        } else if (id.equals(FBinaryOperator.Arith.MODULO.identifier)) {
            return LLVMBuildSRem(builder, left, right, "srem");
        } else if (id.equals(FBinaryOperator.Arith.AND.identifier)) {
            return LLVMBuildAnd(builder, left, right, "and");
        } else if (id.equals(FBinaryOperator.Arith.OR.identifier)) {
            return LLVMBuildOr(builder, left, right, "or");
        } else if (id.equals(FBinaryOperator.Arith.XOR.identifier)) {
            return LLVMBuildXor(builder, left, right, "xor");
        } else if (id.equals(FBinaryOperator.Bool.LESS.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntSLT, left, right, "slt");
        } else if (id.equals(FBinaryOperator.Bool.GREATER.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntSGT, left, right, "sgt");
        } else if (id.equals(FBinaryOperator.Bool.LESS_EQUAL.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntSLE, left, right, "sle");
        } else if (id.equals(FBinaryOperator.Bool.GREATER_EQUAL.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntSGE, left, right, "sge");
        } else if (id.equals(FBinaryOperator.Bool.EQUALS_ID.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntEQ, left, right, "eq");
        } else if (id.equals(FBinaryOperator.Bool.NOT_EQUALS_ID.identifier)) {
            return LLVMBuildICmp(builder, LLVMIntNE, left, right, "ne");
        } else {
            return Utils.cantHappen();
        }
    }

    private LLVMValueRef predefinedArray (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function.isConstructor()) {
            if (((FArray) functionCall.getType()).getDepth() == 1) {
                LLVMTypeRef arrayType = module.getLlvmType(functionCall.getType());

                //compute the array size
                LLVMValueRef sizeRef = Iterables.getOnlyElement(functionCall.getArguments()).accept(this);
                PointerPointer<LLVMValueRef> indices = new PointerPointer<>(
                        LLVMConstInt(module.getLlvmType(FIntN._32), 0, FALSE),
                        LLVMConstInt(module.getLlvmType(FIntN._32), 1, FALSE),
                        sizeRef
                );

                LLVMValueRef nullr = LLVMConstNull(arrayType);
                LLVMValueRef sizeAsPointer = LLVMBuildGEP(builder, nullr, indices, 3, "sizeAsPointer");
                LLVMValueRef size = LLVMBuildPtrToInt(builder, sizeAsPointer, module.getLlvmType(FIntN._64), "size");
                LLVMValueRef malloc = LLVMBuildArrayMalloc(builder, module.getLlvmType(FIntN._8), size, "arrayMalloc");
                LLVMValueRef arrayRef = LLVMBuildBitCast(builder, malloc, arrayType, "newArray");

                //store size
                LLVMValueRef sizeAddress = LLVMBuildStructGEP(builder, arrayRef, 0, "sizeAddress");
                LLVMBuildStore(builder, sizeRef, sizeAddress);
                return arrayRef;
            } else
                return Utils.NYI("multidimensional array constructors");
        } else
            return Utils.NYI(function.headerToString() + "in the backend");
    }

    private LLVMValueRef predefinedIO (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function.getIdentifier() == IOClass.PUTCHAR_ID) {
            LLVMValueRef func = LLVMGetNamedFunction(module.getModule(), "putchar");
            LLVMValueRef arg = getOnlyElement(functionCall.getArguments()).accept(this);
            return LLVMBuildCall(builder, func, arg, 1, new BytePointer(""));
        } else {
            return Utils.cantHappen();
        }
    }

    private LLVMValueRef predefinedFunctionCall (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function instanceof FUnaryOperator) {
            return predefinedUnary(functionCall);
        } else if (function instanceof FBinaryOperator) {
            return predefinedBinary(functionCall);
        } else if (function.getClazz() instanceof FArray) {
            return predefinedArray(functionCall);
        } else if (function.getClazz() instanceof FArray) {
                return predefinedArray(functionCall);
        } else if (function.getClazz() == IOClass.INSTANCE) {
            return predefinedIO(functionCall);
        } else {
            return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitFunctionCall(FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function.isPredefined())
            return predefinedFunctionCall(functionCall);

        LLVMValueRef func = LLVMGetNamedFunction(module.getModule(), getFunctionName(function));
        List<LLVMValueRef> args = new ArrayList<>();
        //this parameter for non-static
        if (!function.isStatic())
            args.add(functionCall.getObject().accept(this));
        //given arguments
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        List<FParameter> params = function.getParams();
        //use default values for non specified parameters
        for (int i=functionCall.getArguments().size(); i<params.size(); i++)
            args.add(params.get(i).getDefaultValue().get().accept(this));
        String instructionName = function.getType() == FVoid.INSTANCE ? "" : "callTmp";
        return LLVMBuildCall(builder, func, createPointerPointer(args), args.size(), instructionName);
    }

    @Override
    public LLVMValueRef visitFieldAccess(FFieldAccess fieldAccess) {
        FField field = fieldAccess.getField();
        LLVMValueRef address;
        if (field.isStatic()) {
            address = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(field));
        } else {
            LLVMValueRef object = fieldAccess.getObject().accept(this);
            address = LLVMBuildStructGEP(builder, object, module.getFieldIndex(field), "GEP_" + field.getIdentifier().name);
        }
        switch (fieldAccess.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, "load_" + field.getIdentifier().name);
            case STORE:
                return address;
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitLiteral(FLiteralExpression expression) {
        FLiteral literal = expression.getLiteral();
        LLVMTypeRef type = module.getLlvmType(literal.getType());
        if (literal instanceof FInt32Literal) {
            return LLVMConstInt(type, ((FInt32Literal)literal).value, TRUE);
        } else if (literal instanceof FInt64Literal) {
            return LLVMConstInt(type, ((FInt64Literal) literal).value, TRUE);
        } else if (literal instanceof FFloat32Literal) {
            return LLVMConstRealOfString(type, ((FFloat32Literal)literal).originalString);
        } else if (literal instanceof FFloat64Literal) {
            return LLVMConstRealOfString(type, ((FFloat64Literal) literal).originalString);
        } else if (literal instanceof FCharLiteral) {
            return LLVMConstInt(type, ((FCharLiteral) literal).value, FALSE);
        } else if (literal instanceof FStringLiteral) {
            return module.constantString(((FStringLiteral) literal).value);
        } else if (literal == FBoolLiteral.TRUE) {
            return LLVMConstInt(type, TRUE, FALSE);
        } if (literal == FBoolLiteral.FALSE) {
            return LLVMConstInt(type, FALSE, FALSE);
        } else if (literal == FNull.INSTANCE) {
            //return LLVMConstNull()
            //return LLVMConstPointerNull()
            return Utils.NYI("null");
        } else {
            return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitVariable(FLocalVariableExpression expression) {
        LLVMValueRef address = localVars.get(expression.getVariable());
        switch (expression.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, expression.getVariable().getIdentifier().name);
            case STORE:
                return address;
            default:
                return Utils.cantHappen();
        }
    }
}
