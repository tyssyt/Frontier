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
import tys.frontier.code.predefinedClasses.FBool;
import tys.frontier.code.predefinedClasses.FIntN;
import tys.frontier.code.predefinedClasses.FVoid;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.ClassWalker;
import tys.frontier.modules.io.IOClass;
import tys.frontier.util.Pair;
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
    private Map<FLoop, Pair<LLVMBasicBlockRef, LLVMBasicBlockRef>> loopJumpPoints = new HashMap<>();

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
    public LLVMValueRef visitField(FField field) {
        LLVMValueRef res = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(field));
        if (res.isNull()) {
            Utils.cantHappen();
        }

        LLVMTypeRef type = module.getLlvmType(field.getType());
        LLVMValueRef val = field.getAssignment()
                .map(FVarAssignment::getValue)
                .filter(v -> v instanceof FLiteralExpression)
                .map(v -> (FLiteralExpression)v)
                .map(lit -> lit.accept(this))
                .orElse(LLVMConstNull(type));
        LLVMSetInitializer(res, val);
        return null;
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

        //for constructors, allocate the this object
        if (function.isConstructor()) {
            LLVMValueRef alloca = createEntryBlockAlloca(function.getClazz().getThis());
            LLVMValueRef malloc = LLVMBuildMalloc(builder, LLVMGetElementType(module.getLlvmType(function.getClazz())), "malloc_" + function.getClazz().getIdentifier());
            LLVMBuildStore(builder, malloc, alloca);
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
        //TODO there might be a bug where the after_if block gets appended but bever used because if.redirectsControlFlow is true
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
        if (!fIf.getThen().redirectsControlFlow().isPresent())
            LLVMBuildBr(builder, continueBlock);

        fIf.getElse().ifPresent(elze -> {
            LLVMPositionBuilderAtEnd(builder, elseBlock);
            elze.accept(this);
            if (!elze.redirectsControlFlow().isPresent())
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
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef condBlock = LLVMAppendBasicBlock(currentFunction, "while_cond");
        LLVMBasicBlockRef bodyBlock = LLVMAppendBasicBlock(currentFunction, "while_body");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after_while");
        loopJumpPoints.put(fWhile, new Pair<>(condBlock, afterBlock));

        LLVMBuildBr(builder, condBlock);

        LLVMPositionBuilderAtEnd(builder, condBlock);
        LLVMValueRef condition = fWhile.getCondition().accept(this);
        LLVMBuildCondBr(builder, condition, bodyBlock, afterBlock);

        LLVMPositionBuilderAtEnd(builder, bodyBlock);
        fWhile.getBody().accept(this);
        if (!fWhile.getBody().redirectsControlFlow().isPresent())
            LLVMBuildBr(builder, condBlock);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        return null;
    }

    @Override
    public LLVMValueRef visitFor(FFor fFor) {
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef condBlock = LLVMAppendBasicBlock(currentFunction, "for_cond");
        LLVMBasicBlockRef bodyBlock = LLVMAppendBasicBlock(currentFunction, "for_body");
        LLVMBasicBlockRef incBlock = LLVMAppendBasicBlock(currentFunction, "for_inc");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after_for");
        loopJumpPoints.put(fFor, new Pair<>(incBlock, afterBlock));

        fFor.getDeclaration().ifPresent(decl -> decl.accept(this));
        LLVMBuildBr(builder, condBlock);

        LLVMPositionBuilderAtEnd(builder, condBlock);
        LLVMValueRef condition = fFor.getCondition().map( c -> c.accept(this)).orElse(boolLiteral(true));
        LLVMBuildCondBr(builder, condition, bodyBlock, afterBlock);

        LLVMPositionBuilderAtEnd(builder, bodyBlock);
        fFor.getBody().accept(this);
        if (!fFor.getBody().redirectsControlFlow().isPresent())
            LLVMBuildBr(builder, incBlock);

        LLVMPositionBuilderAtEnd(builder, incBlock);
        fFor.getIncrement().ifPresent(inc -> inc.accept(this));
        LLVMBuildBr(builder, condBlock);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        return null;
    }

    @Override
    public LLVMValueRef visitForEach(FForEach forEach) {
        return Utils.cantHappen();
    }

    @Override
    public LLVMValueRef visitEmpty(FEmptyStatement statement) {
        return null;
    }

    @Override
    public LLVMValueRef visitBreak(FBreak fBreak) {
        LLVMBasicBlockRef target = loopJumpPoints.get(fBreak.getLoop().getLoop()).b;
        LLVMValueRef res = LLVMBuildBr(builder, target);
        LLVMPositionBuilderAtEnd(builder, target);
        return res;
    }

    @Override
    public LLVMValueRef visitContinue(FContinue fContinue) {
        LLVMBasicBlockRef target = loopJumpPoints.get(fContinue.getLoop().getLoop()).a;
        LLVMValueRef res = LLVMBuildBr(builder, target);
        LLVMPositionBuilderAtEnd(builder, target);
        return res;
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
        else if (id.equals(FUnaryOperator.Pre.INC.identifier))
            return incDec(arg, LLVMAdd, true);
        else if (id.equals(FUnaryOperator.Pre.DEC.identifier))
            return incDec(arg, LLVMSub, true);
        else if (id.equals(FUnaryOperator.Post.INC.identifier))
            return incDec(arg, LLVMAdd, false);
        else if (id.equals(FUnaryOperator.Post.DEC.identifier))
            return incDec(arg, LLVMSub, false);
        else
            return Utils.cantHappen();
    }

    private LLVMValueRef incDec(LLVMValueRef addr, int op, boolean pre) {
        LLVMValueRef load = LLVMBuildLoad(builder, addr, "load_incdec");
        LLVMValueRef modified = LLVMBuildBinOp(builder, op, load, LLVMConstInt(LLVMTypeOf(load), 1, TRUE), "incdec");
        LLVMBuildStore(builder, modified, addr);
        return pre ? load : modified;
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

        //TODO create an enum that maps Id to the llvm op and actually make this all a 1 liner...
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
        if (literal instanceof FIntNLiteral) {
            return intLiteral(((FIntNLiteral) literal).value.longValue(), ((FIntNLiteral) literal).type.getN());
        } else if (literal instanceof FFloat32Literal) {
            return LLVMConstRealOfString(type, ((FFloat32Literal)literal).originalString);
        } else if (literal instanceof FFloat64Literal) {
            return LLVMConstRealOfString(type, ((FFloat64Literal) literal).originalString);
        } else if (literal instanceof FCharLiteral) {
            return intLiteral(((FCharLiteral) literal).value, 8);
        } else if (literal instanceof FStringLiteral) {
            LLVMValueRef res = module.constantString(((FStringLiteral) literal).value);
            return LLVMBuildBitCast(builder, res, type, ""); //cast to get rid of the explicit length in the array type to make LLVM happy
        } else if (literal instanceof  FBoolLiteral) {
            return boolLiteral(((FBoolLiteral) literal).value);
        } else if (literal == FNull.INSTANCE) {
            //return LLVMConstNull()
            //return LLVMConstPointerNull()
            return Utils.NYI("null");
        } else {
            return Utils.cantHappen();
        }
    }

    private LLVMValueRef intLiteral(long i, int bits) {
        return LLVMConstInt(module.getLlvmType(FIntN.getIntN(bits)), i, TRUE);
    }

    private LLVMValueRef boolLiteral(boolean b) {
        return LLVMConstInt(module.getLlvmType(FBool.INSTANCE), b ? TRUE : FALSE, FALSE);
    }

    @Override
    public LLVMValueRef visitVariable(FLocalVariableExpression expression) {
        LLVMValueRef address = localVars.get(expression.getVariable());
        switch (expression.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, expression.getVariable().getIdentifier().name);
            case STORE: case LOAD_AND_STORE:
                return address;
            default:
                return Utils.cantHappen();
        }
    }
}
