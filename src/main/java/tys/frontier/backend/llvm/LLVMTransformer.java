package tys.frontier.backend.llvm;

import com.google.common.collect.Iterables;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.*;
import tys.frontier.code.Operator.FBinaryOperator;
import tys.frontier.code.Operator.FUnaryOperator;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.ClassWalker;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

    private final LLVMTypeRef indexType;

    public LLVMTransformer(LLVMModule module) {
        this.module = module;
        this.builder = module.createBuilder();
        this.entryBlockAllocaBuilder = module.createBuilder();
        indexType = module.getLlvmType(FIntN._32);
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
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(module.sfInit));
        if (res.isNull()) {
            Utils.cantHappen();
        }

        if (field.hasAssignment()) {
            @SuppressWarnings("OptionalGetWithoutIsPresent")
            LLVMValueRef val = field.getAssignment().get().accept(this);
            if (LLVMIsConstant(val) == TRUE) {
                LLVMSetInitializer(res, val);
            } else {
                LLVMSetInitializer(res, LLVMConstNull(module.getLlvmType(field.getType())));
                LLVMBuildStore(builder, val, res);
            }
        } else
            LLVMSetInitializer(res, LLVMConstNull(module.getLlvmType(field.getType())));
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
        List<FParameter> fParams = function.getParams();
        for (int i=0; i<fParams.size(); i++) {
            LLVMValueRef alloca = createEntryBlockAlloca(fParams.get(i));
            LLVMBuildStore(builder, LLVMGetParam(res, i), alloca);
        }

        //do the body
        //noinspection ConstantConditions,OptionalGetWithoutIsPresent
        for (FStatement statement : function.getBody().get())
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
        for (FStatement statement : block)
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
        LLVMValueRef array = arrayAccess.getObject().accept(this);
        LLVMValueRef index = arrayAccess.getIndex().accept(this);
        LLVMValueRef address = arrayGep(array, index);

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
                return LLVMBuildSExt(builder, toCast, targetType, "cast_int_prom");
            case FLOAT_PROMOTION:
                return LLVMBuildFPExt(builder, toCast, targetType, "cast_float_prom");
            case TO_OPTIONAL:
                if (implicitCast.getCastedExpression().getType() == FBool.INSTANCE)
                    return LLVMBuildZExt(builder, toCast, targetType, "cast_bool_opt");
                return toCast;
            case OPTIONAL_TO_BOOL:
                return LLVMBuildICmp(builder, LLVMIntNE, toCast, module.getNull((FOptional) implicitCast.getCastedExpression().getType()), "ne");
            case DELEGATE:
                List<FField> path = ((FClass) implicitCast.getCastedExpression().getType()).getDelegate(implicitCast.getType());
                LLVMValueRef cur = toCast;
                for (FField field : path) {
                    LLVMValueRef addr = LLVMBuildStructGEP(builder, cur, module.getFieldIndex(field), "GEP_delegate_" + field.getIdentifier().name);
                    cur = LLVMBuildLoad(builder, addr, "load_delegate_" + field.getIdentifier().name);
                }
                return cur;
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
                return LLVMBuildTrunc(builder, toCast, targetType, "cast_int_dem");
            case FLOAT_DEMOTION:
                return LLVMBuildFPTrunc(builder, toCast, targetType, "cast_float_dem");
            case FLOAT_TO_INT:
                return LLVMBuildFPToSI(builder, toCast, targetType, "cast_float_int");
            case INT_TO_FLOAT:
                return LLVMBuildSIToFP(builder, toCast, targetType, "cast_int_float");
            case REMOVE_OPTIONAL:
                //TODO when we have some sort of runtime errors, check before casting and throw errors (and then see if we can avoid checking next)
                //return LLVMBuildICmp(builder, LLVMIntEQ, toCast, module.getNull((FOptional) explicitCast.getCastedExpression().getType()), "check_NPE");
                if (explicitCast.getType() == FBool.INSTANCE)
                    return LLVMBuildTrunc(builder, toCast, targetType, "bool!");
                return toCast;
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitOptElse(FOptElse optElse) {
        LLVMValueRef optional = optElse.getOptional().accept(this);

        LLVMValueRef cond = LLVMBuildICmp(builder, LLVMIntNE, optional, module.getNull((FOptional) optElse.getOptional().getType()), "checkNull");
        LLVMValueRef then;
        if (optElse.getType() == FBool.INSTANCE) //TODO this is copy paste from explicit cast
            then = LLVMBuildTrunc(builder, optional, module.getLlvmType(FBool.INSTANCE), "bool!");
        else
            then = optional;
        LLVMValueRef elze = optElse.getElse().accept(this);
        return LLVMBuildSelect(builder, cond, then, elze, "ifExpr");
    }

    private LLVMValueRef predefinedUnary(FFunctionCall functionCall) {
        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        LLVMValueRef arg = functionCall.getArguments().get(0).accept(this);
        if (id.equals(FUnaryOperator.Pre.NOT.identifier))
            return LLVMBuildNot(builder, arg, "not");
        else if (id.equals(FUnaryOperator.Pre.NEG.identifier))
            return LLVMBuildNeg(builder, arg, "neg");
        else if (id.equals(FUnaryOperator.Pre.INC.identifier))
            return incDec(arg, LLVMAdd);
        else if (id.equals(FUnaryOperator.Pre.DEC.identifier))
            return incDec(arg, LLVMSub);
        else
            return Utils.cantHappen();
    }

    private LLVMValueRef incDec(LLVMValueRef addr, int op) {
        LLVMValueRef load = LLVMBuildLoad(builder, addr, "load_incdec");
        LLVMValueRef modified = LLVMBuildBinOp(builder, op, load, LLVMConstInt(LLVMTypeOf(load), 1, TRUE), "incdec");
        LLVMBuildStore(builder, modified, addr);
        return modified;
    }

    private LLVMValueRef predefinedBinary(FFunctionCall functionCall) {
        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        FExpression l = functionCall.getArguments().get(0);
        FExpression r = functionCall.getArguments().get(1);

        if (id.equals(FBinaryOperator.Bool.AND.identifier))
            return shortCircuitLogic(l, r, true);
        else if (id.equals(FBinaryOperator.Bool.OR.identifier))
            return shortCircuitLogic(l, r, false);

        LLVMValueRef left = l.accept(this);
        LLVMValueRef right = r.accept(this);
        //TODO create an enum that maps Id to the llvm op and actually make this all a 1 liner... (or not)
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

    private LLVMValueRef shortCircuitLogic(FExpression first, FExpression second, boolean isAnd) {
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef startBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef otherBlock = LLVMAppendBasicBlock(currentFunction, "other");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after");

        LLVMValueRef f = first.accept(this);
        if (isAnd)
            LLVMBuildCondBr(builder, f, otherBlock, afterBlock);
        else
            LLVMBuildCondBr(builder, f, afterBlock, otherBlock);

        LLVMPositionBuilderAtEnd(builder, otherBlock);
        LLVMValueRef s = second.accept(this);
        LLVMBuildBr(builder, afterBlock);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(FBool.INSTANCE), "ss_logic");
        LLVMAddIncoming(phi, f, startBlock, 1);
        LLVMAddIncoming(phi, s, otherBlock, 1);
        return phi;
    }

    private LLVMValueRef predefinedArray (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function.isConstructor()) {
            LLVMTypeRef arrayType = module.getLlvmType(functionCall.getType());

            //compute the array size
            LLVMValueRef sizeRef = Iterables.getOnlyElement(functionCall.getArguments()).accept(this);

            LLVMValueRef size = arrayOffsetOf(arrayType, sizeRef);
            LLVMValueRef malloc = LLVMBuildArrayMalloc(builder, module.byteType, size, "arrayMalloc");
            LLVMValueRef arrayRef = LLVMBuildBitCast(builder, malloc, arrayType, "newArray");

            //store size
            LLVMValueRef sizeAddress = LLVMBuildStructGEP(builder, arrayRef, 0, "sizeAddress");
            LLVMBuildStore(builder, sizeRef, sizeAddress);
            return arrayRef;
        } else
            return Utils.NYI(function.headerToString() + " in the backend");
    }

    private LLVMValueRef predefinedOptional (FFunctionCall functionCall) { //TODO this is copy & paste from if and call...
        FFunction function = functionCall.getFunction();
        FOptional optional = (FOptional) function.getMemberOf();
        FFunction toCall = optional.getShimMap().inverse().get(function);
        assert toCall != null;

        List<? extends FExpression> fArgs = functionCall.getArguments();
        LLVMValueRef This = fArgs.get(0).accept(this);

        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef originalBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef thenBlock = LLVMAppendBasicBlock(currentFunction, "call_nonnull");
        LLVMBasicBlockRef continueBlock = LLVMAppendBasicBlock(currentFunction, "after_call");

        LLVMValueRef If = LLVMBuildICmp(builder, LLVMIntNE, This, module.getNull(optional), "checkNull");
        LLVMBuildCondBr(builder, If, thenBlock, continueBlock);

        LLVMPositionBuilderAtEnd(builder, thenBlock);
        //call
        LLVMValueRef func = LLVMGetNamedFunction(module.getModule(), getFunctionName(toCall));
        List<LLVMValueRef> llvmArgs = new ArrayList<>();
        //this parameter
        llvmArgs.add(This);
        //given arguments besides "this"
        for (int i = 1; i < fArgs.size(); i++)
            llvmArgs.add(fArgs.get(i).accept(this));
        List<FParameter> params = toCall.getParams();
        //use default values for non specified parameters
        for (int i = fArgs.size(); i<params.size(); i++)
            //noinspection ConstantConditions,OptionalGetWithoutIsPresent
            llvmArgs.add(params.get(i).getDefaultValue().get().accept(this));
        String instructionName = toCall.getType() == FVoid.INSTANCE ? "" : "callTmp";
        LLVMValueRef call = LLVMBuildCall(builder, func, createPointerPointer(llvmArgs), llvmArgs.size(), instructionName);
        LLVMBuildBr(builder, continueBlock);

        LLVMPositionBuilderAtEnd(builder, continueBlock);
        if (function.getType() == FVoid.INSTANCE) {
            return null;
        } else {
            LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(function.getType()), "phi_optcall");
            LLVMAddIncoming(phi, call, thenBlock, 1);
            LLVMAddIncoming(phi, module.getNull((FOptional) function.getType()), originalBlock, 1); //TODO not sure if I should return This or null, if any of them helps the optimizer?
            return phi;
        }
    }

    private LLVMValueRef predefinedFunctionCall (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function instanceof FUnaryOperator) {
            return predefinedUnary(functionCall);
        } else if (function instanceof FBinaryOperator) {
            return predefinedBinary(functionCall);
        } else if (function.getMemberOf() instanceof FArray) {
            return predefinedArray(functionCall);
        } else if (function.getMemberOf() instanceof FOptional) {
            return predefinedOptional(functionCall);
        } else if (function.getIdentifier().equals(FConstructor.MALLOC_ID)) {
            return LLVMBuildMalloc(builder, LLVMGetElementType(module.getLlvmType(functionCall.getType())), "malloc_" + functionCall.getType().getIdentifier());
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
        assert func != null && !func.isNull() : function.getIdentifier() + "could not be found in module";
        List<LLVMValueRef> args = new ArrayList<>();
        //given arguments
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        List<FParameter> params = function.getParams();
        //use default values for non specified parameters
        for (int i=functionCall.getArguments().size(); i<params.size(); i++)
            //noinspection ConstantConditions,OptionalGetWithoutIsPresent
            args.add(params.get(i).getDefaultValue().get().accept(this));
        String instructionName = function.getType() == FVoid.INSTANCE ? "" : "callTmp";
        return LLVMBuildCall(builder, func, createPointerPointer(args), args.size(), instructionName);
    }

    @Override
    public LLVMValueRef visitDynamicFunctionCall(DynamicFunctionCall functionCall) {
        //TODO this doesn't work for predefined functions
        LLVMValueRef function = functionCall.getFunction().accept(this);
        List<LLVMValueRef> args = new ArrayList<>();
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        String instructionName = functionCall.getType() == FVoid.INSTANCE ? "" : "callTmp";
        return LLVMBuildCall(builder, function, createPointerPointer(args), args.size(), instructionName);
    }

    @Override
    public LLVMValueRef visitFieldAccess(FFieldAccess fieldAccess) {
        FField field = fieldAccess.getField();
        LLVMValueRef address;
        if (field.isInstance()) {
            LLVMValueRef object = fieldAccess.getObject().accept(this);
            address = LLVMBuildStructGEP(builder, object, module.getFieldIndex(field), "GEP_" + field.getIdentifier().name);
        } else {
            address = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(field));
        }
        switch (fieldAccess.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, "load_" + field.getIdentifier().name);
            case STORE: case LOAD_AND_STORE:
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
            return LLVMConstInt(type, ((FIntNLiteral) literal).value.longValue(), TRUE);
        } else if (literal instanceof FFloat32Literal) {
            return LLVMConstRealOfString(type, ((FFloat32Literal)literal).originalString);
        } else if (literal instanceof FFloat64Literal) {
            return LLVMConstRealOfString(type, ((FFloat64Literal) literal).originalString);
        } else if (literal instanceof FCharLiteral) {
            return LLVMConstInt(type, ((FCharLiteral) literal).value, TRUE);
        } else if (literal instanceof FStringLiteral) {
            LLVMValueRef res = module.constantString(((FStringLiteral) literal).value);
            return LLVMBuildBitCast(builder, res, type, ""); //cast to get rid of the explicit length in the array type to make LLVM happy
        } else if (literal instanceof  FBoolLiteral) {
            return LLVMConstInt(type, ((FBoolLiteral) literal).value ? TRUE : FALSE, FALSE);
        } else if (literal instanceof FNull) {
            return module.getNull((FOptional)literal.getType());
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
            case STORE: case LOAD_AND_STORE:
                return address;
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitClassExpr(FClassExpression expression) {
        return LLVMConstPointerNull(module.getLlvmType(expression.getType())); //TODO this is where we would want to have RTTI but we don't
    }

    @Override
    public LLVMValueRef visitFunctionAddress(FFunctionAddress expression) {
        return LLVMGetNamedFunction(module.getModule(), getFunctionName(expression.getFunction()));
    }

    private LLVMValueRef intLiteral(long i, int bits) {
        return LLVMConstInt(module.getLlvmType(FIntN.getIntN(bits)), i, TRUE);
    }

    private LLVMValueRef boolLiteral(boolean b) {
        return LLVMConstInt(module.getLlvmType(FBool.INSTANCE), b ? TRUE : FALSE, FALSE);
    }

    private LLVMValueRef indexLiteral(int i) {
        return LLVMConstInt(indexType, i, FALSE);
    }

    private LLVMValueRef arrayGep(LLVMValueRef value, LLVMValueRef index) {
        PointerPointer<LLVMValueRef> indices = new PointerPointer<>(indexLiteral(0), indexLiteral(1), index);
        return LLVMBuildGEP(builder, value, indices, 3, "GEP_array");
    }

    private LLVMValueRef offsetOf(LLVMTypeRef type, int index) {
        LLVMValueRef asPointer = LLVMBuildStructGEP(builder, LLVMConstNull(type), index, "GEP");
        return LLVMBuildPtrToInt(builder, asPointer, module.getLlvmType(FIntN._64), "offsetOf");
    }

    private LLVMValueRef arrayOffsetOf(LLVMTypeRef type, LLVMValueRef index) {
        LLVMValueRef asPointer = arrayGep(LLVMConstNull(type), index);
        return LLVMBuildPtrToInt(builder, asPointer, module.getLlvmType(FIntN._64), "offsetOf_array");
    }
}
