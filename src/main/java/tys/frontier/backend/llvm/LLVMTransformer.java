package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.*;
import org.bytedeco.llvm.global.LLVM;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.*;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FieldAccessor;
import tys.frontier.code.function.operator.Access;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.namespace.OptionalNamespace;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassWalker;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.util.NameGenerator;
import tys.frontier.util.OS;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.math.BigInteger;
import java.util.*;

import static com.google.common.collect.Iterables.getOnlyElement;
import static java.util.Collections.emptyList;
import static org.bytedeco.llvm.global.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;
import static tys.frontier.code.function.operator.BinaryOperator.*;

class LLVMTransformer implements
        AutoCloseable,
        ClassWalker<LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef> {

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    private static final ImmutableMap<FIdentifier, Integer> arithOpMap = ImmutableMap.<FIdentifier, Integer>builder()
            .put(PLUS.identifier, LLVMAdd)
            .put(MINUS.identifier, LLVMSub)
            .put(TIMES.identifier, LLVMMul)
            .put(DIVIDED.identifier, LLVMSDiv)
            .put(MODULO.identifier, LLVMSRem)
            .put(AAND.identifier, LLVMAnd)
            .put(AOR.identifier, LLVMOr)
            .put(XOR.identifier, LLVMXor)
            .put(FIntN.SHIFT_L, LLVMShl)
            .put(FIntN.U_SHIFT_R, LLVMLShr)
            .put(FIntN.S_SHIFT_R, LLVMAShr)
            .build();
    private static final ImmutableMap<FIdentifier, Integer> cmpOpMap = ImmutableMap.<FIdentifier, Integer>builder()
            .put(EQUALS.identifier, LLVMIntEQ)
            .put(EQUALS_ID.identifier, LLVMIntEQ)
            .put(NOT_EQUALS.identifier, LLVMIntNE)
            .put(NOT_EQUALS_ID.identifier, LLVMIntNE)
            .put(LESS.identifier, LLVMIntSLT)
            .put(GREATER.identifier, LLVMIntSGT)
            .put(LESS_EQUAL.identifier, LLVMIntSLE)
            .put(GREATER_EQUAL.identifier, LLVMIntSGE)
            .build();
    private static final ImmutableMap<FIdentifier, Integer> arithFOpMap = ImmutableMap.<FIdentifier, Integer>builder()
            .put(PLUS.identifier, LLVMFAdd)
            .put(MINUS.identifier, LLVMFSub)
            .put(TIMES.identifier, LLVMFMul)
            .put(DIVIDED.identifier, LLVMFDiv)
            .put(MODULO.identifier, LLVMFRem)
            .build();
    private static final ImmutableMap<FIdentifier, Integer> cmpFOpMap = ImmutableMap.<FIdentifier, Integer>builder()
            .put(EQUALS.identifier, LLVMRealOEQ)
            .put(EQUALS_ID.identifier, LLVMRealOEQ)
            .put(NOT_EQUALS.identifier, LLVMRealONE)
            .put(NOT_EQUALS_ID.identifier, LLVMRealONE)
            .put(LESS.identifier, LLVMRealOLT)
            .put(GREATER.identifier, LLVMRealOGT)
            .put(LESS_EQUAL.identifier, LLVMRealOLE)
            .put(GREATER_EQUAL.identifier, LLVMRealOGE)
            .build();

    private static final ImmutableMap<FIdentifier, String> floatIntrinsics = ImmutableMap.<FIdentifier, String>builder()
            .put(FFloat.LOG, "llvm.log")
            .put(FFloat.LOG10, "llvm.log10")
            .put(FFloat.LOG2, "llvm.log2")
            .put(FFloat.CEIL, "llvm.ceil")
            .put(FFloat.FLOOR, "llvm.floor")
            .put(FFloat.TRUNC, "llvm.trunc")
            .build();

    private LLVMModule module;
    private LLVMBuilderRef builder;
    private LLVMBuilderRef entryBlockAllocaBuilder;
    private LLVMDIBuilderRef diBuilder; //null when debug is disabled
    private Map<FLocalVariable, LLVMValueRef> localVars = new HashMap<>();
    private Map<FLocalVariable, LLVMValueRef> tempVars = new HashMap<>(); //TODO this is a bit of a hack to avoid creating vars in function calls
    private Map<FLoop, Pair<LLVMBasicBlockRef, LLVMBasicBlockRef>> loopJumpPoints = new HashMap<>();

    private final LLVMTypeRef indexType;

    private LLVMValueRef sfInit;
    private LLVMValueRef cStringToFString;

    private LLVMMetadataRef debugScope;


    public LLVMTransformer(LLVMModule module, FFunction entryPoint, boolean debug) {
        this.module = module;
        this.builder = module.createBuilder();
        this.entryBlockAllocaBuilder = module.createBuilder();
        indexType = module.getLlvmType(FIntN._32);

        if (debug) {
            this.diBuilder = module.createDebugInfoBuilder();
            LLVMMetadataRef fileScope = createFileScope(entryPoint.getLocation());
            LLVMDIBuilderCreateCompileUnit(diBuilder, LLVMDWARFSourceLanguageC, fileScope, "me :)", 5, FALSE, "", 0, 0, "", 0, LLVMDWARFEmissionFull, 0, TRUE, FALSE);
            if (OS.isWindows()) {
                //TODO no idea what the behaviour does
                LLVMAddModuleFlag(module.getModule(), LLVMModuleFlagBehaviorWarning, "CodeView", 8, LLVMValueAsMetadata(indexLiteral(1)));
                LLVMAddModuleFlag(module.getModule(), LLVMModuleFlagBehaviorWarning, "Debug Info Version", 18, LLVMValueAsMetadata(indexLiteral(3)));
            }
        }

        sfInit = LLVMAddFunction(module.getModule(), "sf.init", LLVMFunctionType(module.getLlvmType(FTuple.VOID), (PointerPointer<LLVMTypeRef>) null, 0, FALSE));
        LLVMAppendBasicBlock(sfInit, "entry");
        cStringToFString = createCStringToFString(); //TODO this is not the ideal place to create this function
    }

    @Override
    public void close() {
        if (diBuilder != null) {
            LLVMDIBuilderFinalize(diBuilder);
            LLVMDisposeDIBuilder(diBuilder);
        }
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

    public void generateWinMain(FFunction entryPoint, FField hInstance, FField nCmdShow) { //TODO reduce copy paste with generateMain
        LLVMTypeRef ptr = LLVMPointerType(LLVMStructType((PointerPointer<LLVMTypeRef>) null, 0, FALSE), 0);
        PointerPointer<LLVMTypeRef> argTypes = createPointerPointer(
                ptr,
                ptr,
                LLVMPointerType(module.getLlvmType(FIntN._8), 0),
                indexType
        );
        LLVMTypeRef functionType = LLVMFunctionType(indexType, argTypes, 4, FALSE);

        LLVMValueRef function = LLVMAddFunction(module.getModule(), "WinMain", functionType);
        debugScope = createFunctionDebugInfo(entryPoint, function);
        setDebugLocation(entryPoint.getLocation().getPoint());
        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(function, "alloca");
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(function, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);
        LLVMBuildCall(builder, sfInit, null, 0, "");

        //init hInstance & nCmdShow
        if (hInstance != null) {
            LLVMValueRef address = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(hInstance));
            LLVMBuildStore(builder, LLVMBuildBitCast(builder, LLVMGetParam(function, 0), LLVMGetElementType(LLVMTypeOf(address)), "c"), address);
        }
        if (nCmdShow != null) {
            LLVMValueRef address = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(nCmdShow));
            LLVMBuildStore(builder, LLVMBuildBitCast(builder, LLVMGetParam(function, 3), LLVMGetElementType(LLVMTypeOf(address)), "c"), address);
        }

        //call entry Point
        LLVMValueRef userMain = LLVMGetNamedFunction(module.getModule(), getFunctionName(entryPoint));
        if (entryPoint.getSignature().getParameters().isEmpty()) {
            LLVMBuildCall(builder, userMain, null, 0, "");
        } else {
            //convert input to Frontier String
            LLVMValueRef args = convertArg(function, LLVMGetParam(function, 2));
            LLVMBuildCall(builder, userMain, createPointerPointer(args), 1, "");
        }
        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));
        LLVMBuildBr(entryBlockAllocaBuilder, entryBlock);

        //finish sfInit
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(sfInit));
        LLVMBuildRetVoid(builder);
    }

    public void generateMain(FFunction entryPoint) {
        PointerPointer<LLVMTypeRef> argTypes = createPointerPointer(indexType,
                LLVMPointerType(LLVMPointerType(module.getLlvmType(FIntN._8), 0), 0)
        );
        LLVMTypeRef functionType = LLVMFunctionType(indexType, argTypes, 2, FALSE);

        LLVMValueRef function = LLVMAddFunction(module.getModule(), "main", functionType);
        debugScope = createFunctionDebugInfo(entryPoint, function);
        setDebugLocation(entryPoint.getLocation().getPoint());
        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(function, "alloca");
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(function, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);
        LLVMBuildCall(builder, sfInit, null, 0, "");

        //call entry Point
        LLVMValueRef userMain = LLVMGetNamedFunction(module.getModule(), getFunctionName(entryPoint));
        if (entryPoint.getSignature().getParameters().isEmpty()) {
            LLVMBuildCall(builder, userMain, null, 0, "");
        } else {
            //convert input to Frontier String
            LLVMValueRef args = convertArgs(function, LLVMGetParam(function, 0), LLVMGetParam(function, 1));
            LLVMBuildCall(builder, userMain, createPointerPointer(args), 1, "");
        }
        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));
        LLVMBuildBr(entryBlockAllocaBuilder, entryBlock);

        //finish sfInit
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(sfInit));
        LLVMBuildRetVoid(builder);
    }

    private LLVMValueRef convertArg(LLVMValueRef function, LLVMValueRef lpCmdLine) { //TODO split lpCmdLine into words
        LLVMTypeRef fStringType = module.getLlvmType(FArray.getArrayFrom(FStringLiteral.TYPE));

        //TODO this is a alloca copy of arrayMalloc
        LLVMValueRef size = arrayOffsetOf(fStringType, indexLiteral(1));
        LLVMValueRef alloca = LLVMBuildArrayAlloca(builder, module.byteType, size, "arrayAlloca");
        LLVMValueRef res = LLVMBuildBitCast(builder, alloca, fStringType, "newArray");

        //store size
        LLVMValueRef sizeAddress = LLVMBuildStructGEP(builder, res, 0, "sizeAddress");
        LLVMBuildStore(builder, indexLiteral(1), sizeAddress);

        //transform arg
        LLVMValueRef arg = LLVMBuildCall(builder, cStringToFString, createPointerPointer(lpCmdLine), 1, "cString2FString");

        //store arg
        LLVMValueRef argAddress = arrayGep(res, indexLiteral(0));
        LLVMBuildStore(builder, arg, argAddress);
        return res;
    }

    private LLVMValueRef convertArgs(LLVMValueRef function, LLVMValueRef argi, LLVMValueRef argv) {
        LLVMTypeRef fStringType = module.getLlvmType(FArray.getArrayFrom(FStringLiteral.TYPE));

        LLVMBasicBlockRef copyBlock = LLVMAppendBasicBlock(function, "copy");
        LLVMBasicBlockRef copy2Block = LLVMAppendBasicBlock(function, "copy2");
        LLVMBasicBlockRef endBlock = LLVMAppendBasicBlock(function, "end");

        LLVMValueRef i = LLVMBuildAlloca(entryBlockAllocaBuilder, indexType, "alloc_i");

        //TODO this is a alloca copy of arrayMalloc
        LLVMValueRef size = arrayOffsetOf(fStringType, argi);
        LLVMValueRef alloca = LLVMBuildArrayAlloca(builder, module.byteType, size, "arrayAlloca");
        LLVMValueRef res = LLVMBuildBitCast(builder, alloca, fStringType, "newArray");

        //store size
        LLVMValueRef sizeAddress = LLVMBuildStructGEP(builder, res, 0, "sizeAddress");
        LLVMBuildStore(builder, argi, sizeAddress);

        LLVMBuildStore(builder, indexLiteral(0), i);
        LLVMBuildBr(builder, copyBlock);

        //copy
        LLVMPositionBuilderAtEnd(builder, copyBlock);
        LLVMValueRef load_i = LLVMBuildLoad(builder, i, "load_i");
        LLVMValueRef _continue = LLVMBuildICmp(builder, LLVMIntULT, load_i, argi, "cont");
        LLVMBuildCondBr(builder, _continue, copy2Block, endBlock);

        LLVMPositionBuilderAtEnd(builder, copy2Block);
        PointerPointer<LLVMValueRef> indices = createPointerPointer(load_i);
        LLVMValueRef stringAddr = LLVMBuildInBoundsGEP(builder, argv, indices, 1, "GEP_string");
        LLVMValueRef string = LLVMBuildLoad(builder, stringAddr, "loadstr");
        LLVMValueRef fString = LLVMBuildCall(builder, cStringToFString, createPointerPointer(string), 1, "cString2FString");
        LLVMValueRef resAddr = arrayGep(res, load_i);
        LLVMBuildStore(builder, fString, resAddr);
        LLVMBuildStore(builder, LLVMBuildAdd(builder, load_i, indexLiteral(1), "inc_i"), i);
        LLVMBuildBr(builder, copyBlock);

        //end
        LLVMPositionBuilderAtEnd(builder, endBlock);
        return res;
    }

    private LLVMValueRef createCStringToFString() {
        LLVMTypeRef charType = LLVMInt8TypeInContext(module.getContext());
        LLVMTypeRef cStringType = LLVMPointerType(charType, 0);
        LLVMTypeRef fStringType = module.getLlvmType(FStringLiteral.TYPE);
        LLVMTypeRef functionType = LLVMFunctionType(fStringType, cStringType, 1, FALSE);

        LLVMValueRef function = LLVMAddFunction(module.getModule(), "cString2fString", functionType);

        LLVMValueRef cString = LLVMGetParam(function, 0);

        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(function, "alloca");
        LLVMBasicBlockRef countLengthBlock = LLVMAppendBasicBlock(function, "countLength");
        LLVMBasicBlockRef allocArrayBlock = LLVMAppendBasicBlock(function, "allocArray");
        LLVMBasicBlockRef copyStringBlock = LLVMAppendBasicBlock(function, "copyString");
        LLVMBasicBlockRef copyString2Block = LLVMAppendBasicBlock(function, "copyString2");
        LLVMBasicBlockRef endBlock = LLVMAppendBasicBlock(function, "end");

        //alloca
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMValueRef i = LLVMBuildAlloca(entryBlockAllocaBuilder, indexType, "alloc_i");
        LLVMBuildStore(entryBlockAllocaBuilder, indexLiteral(-1), i);
        LLVMBuildBr(entryBlockAllocaBuilder, countLengthBlock);

        //count Length
        LLVMPositionBuilderAtEnd(builder, countLengthBlock);
        LLVMValueRef inc = LLVMBuildAdd(builder, LLVMBuildLoad(builder, i, "load_i"), indexLiteral(1), "inc_i");
        LLVMBuildStore(builder, inc, i);
        PointerPointer<LLVMValueRef> indices = createPointerPointer(inc);
        LLVMValueRef charAddr = LLVMBuildInBoundsGEP(builder, cString, indices, 1, "GEP_string");
        LLVMValueRef _char = LLVMBuildLoad(builder, charAddr, "loadChar");
        LLVMValueRef isNull = LLVMBuildICmp(builder, LLVMIntEQ, _char, LLVMConstInt(charType, 0, FALSE), "isNull");
        LLVMBuildCondBr(builder, isNull, allocArrayBlock, countLengthBlock);

        //allocArray
        LLVMPositionBuilderAtEnd(builder, allocArrayBlock);
        LLVMValueRef array = buildArrayMalloc(fStringType, inc);
        LLVMBuildStore(builder, indexLiteral(0), i);
        LLVMBuildBr(builder, copyStringBlock);

        //copyString
        LLVMPositionBuilderAtEnd(builder, copyStringBlock);
        LLVMValueRef load_i = LLVMBuildLoad(builder, i, "load_i");
        indices = createPointerPointer(load_i);
        charAddr = LLVMBuildInBoundsGEP(builder, cString, indices, 1, "GEP_string");
        _char = LLVMBuildLoad(builder, charAddr, "loadChar");
        isNull = LLVMBuildICmp(builder, LLVMIntEQ, _char, LLVMConstInt(charType, 0, FALSE), "isNull");
        LLVMBuildCondBr(builder, isNull, endBlock, copyString2Block);

        LLVMPositionBuilderAtEnd(builder, copyString2Block);
        LLVMValueRef charAddr2 = arrayGep(array, load_i);
        LLVMBuildStore(builder, _char, charAddr2);
        LLVMBuildStore(builder, LLVMBuildAdd(builder, load_i, indexLiteral(1), "inc_i"), i);
        LLVMBuildBr(builder, copyStringBlock);

        //end
        LLVMPositionBuilderAtEnd(builder, endBlock);
        LLVMBuildRet(builder, array);
        return function;
    }

    @Override
    public LLVMValueRef visitField(FField field) {
        LLVMValueRef res = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(field));
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(sfInit));
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
                setDebugLocation(null);
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
        debugScope = createFunctionDebugInfo(function, res);

        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(res, "alloca");
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(res, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);

        //fill in parameters
        setDebugLocation(null);
        List<FParameter> fParams = function.getSignature().getParameters();
        for (int i=0; i<fParams.size(); i++) {
            LLVMValueRef alloca = createEntryBlockAlloca(fParams.get(i));
            LLVMBuildStore(builder, LLVMGetParam(res, i), alloca);
        }

        //do the body
        //noinspection OptionalGetWithoutIsPresent
        for (FStatement statement : function.getBody().get())
            statement.accept(this);

        //finish
        LLVMBuildBr(entryBlockAllocaBuilder, entryBlock);
        if (function.getType() == FTuple.VOID && function.getBody().get().redirectsControlFlow().isEmpty())
            LLVMBuildRetVoid(builder);
        localVars.clear();

        /*
        if (LLVMVerifyFunction(res, 1) == TRUE) {
            LLVMViewFunctionCFG(res);
        }
         */
        return res;
    }

    @Override
    public LLVMValueRef visitBlock(FBlock block) {
        if (block instanceof FLambdaBlock)
            return Utils.NYI("Lambda Block");
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
        boolean hasElse = fIf.getElse().isPresent();
        boolean hasContinue = fIf.redirectsControlFlow().isEmpty();

        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef ifBlock = LLVMAppendBasicBlock(currentFunction, "if");
        LLVMBasicBlockRef thenBlock = LLVMAppendBasicBlock(currentFunction, "then");
        LLVMBasicBlockRef elseBlock = hasElse ? LLVMAppendBasicBlock(currentFunction, "else") : null;
        LLVMBasicBlockRef continueBlock = hasContinue ? LLVMAppendBasicBlock(currentFunction, "after_if") : null;

        setDebugLocation(fIf.getPosition());
        LLVMBuildBr(builder, ifBlock);

        LLVMPositionBuilderAtEnd(builder, ifBlock);
        LLVMValueRef condition = fIf.getCondition().accept(this);
        LLVMBuildCondBr(builder, condition, thenBlock, hasElse ? elseBlock : continueBlock);

        LLVMPositionBuilderAtEnd(builder, thenBlock);
        fIf.getThen().accept(this);
        if (fIf.getThen().redirectsControlFlow().isEmpty())
            LLVMBuildBr(builder, continueBlock);

        fIf.getElse().ifPresent(elze -> {
            LLVMPositionBuilderAtEnd(builder, elseBlock);
            elze.accept(this);
            if (elze.redirectsControlFlow().isEmpty())
                LLVMBuildBr(builder, continueBlock);
        });

        if (hasContinue)
            LLVMPositionBuilderAtEnd(builder, continueBlock);
        return null;
    }

    @Override
    public LLVMValueRef visitReturn(FReturn fReturn) {
        List<LLVMValueRef> values = new ArrayList<>(fReturn.getExpressions().size());
        for (FExpression arg : fReturn.getExpressions())
            values.add(arg.accept(this));
        setDebugLocation(fReturn.getPosition());
        values = prepareArgs(values, fReturn.getArgMapping());

        return switch (values.size()) {
            case 0  -> LLVMBuildRetVoid(builder);
            case 1  -> LLVMBuildRet(builder, getOnlyElement(values));
            default -> LLVMBuildAggregateRet(builder, createPointerPointer(values), values.size());
        };
    }

    @Override
    public LLVMValueRef visitVarAssignment(FAssignment assignment) {
        List<LLVMValueRef> values = new ArrayList<>();
        for (FExpression arg : assignment.getValues())
            values.add(arg.accept(this));
        setDebugLocation(assignment.getPosition()); //TODO maybe thats awkward and should be null instead?
        values = prepareArgs(values, assignment.getArgMapping());

        int i = 0;
        for (FExpression lhsExpression : assignment.getLhsExpressions()) {
            if (lhsExpression instanceof FVariableExpression) {
                FVariableExpression variable = (FVariableExpression) lhsExpression;
                if (variable instanceof FVarDeclaration)
                    createEntryBlockAlloca(variable.getVariable());
                setDebugLocation(variable.getPosition());
                LLVMBuildStore(builder, values.get(i), variable.accept(this));
                i++;
            } else if (lhsExpression instanceof FFunctionCall) {
                FFunctionCall fun = (FFunctionCall) lhsExpression;
                int consumedValues = fun.getSignature().getAssignees().size();
                visitFunctionCall(fun, values.subList(i, i + consumedValues));
                i += consumedValues;
            } else {
                return Utils.cantHappen();
            }
        }
        assert i == values.size();
        return null;
    }

    @Override
    public LLVMValueRef visitWhile(FWhile fWhile) {
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef condBlock = LLVMAppendBasicBlock(currentFunction, "while_cond");
        LLVMBasicBlockRef bodyBlock = LLVMAppendBasicBlock(currentFunction, "while_body");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after_while");
        loopJumpPoints.put(fWhile, new Pair<>(condBlock, afterBlock));

        setDebugLocation(fWhile.getPosition());
        LLVMBuildBr(builder, condBlock);

        LLVMPositionBuilderAtEnd(builder, condBlock);
        LLVMValueRef condition = fWhile.getCondition().accept(this);
        LLVMBuildCondBr(builder, condition, bodyBlock, afterBlock);

        LLVMPositionBuilderAtEnd(builder, bodyBlock);
        fWhile.getBody().accept(this);
        if (fWhile.getBody().redirectsControlFlow().isEmpty())
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
        setDebugLocation(fBreak.getPosition());
        LLVMValueRef res = LLVMBuildBr(builder, target);
        LLVMPositionBuilderAtEnd(builder, target);
        return res;
    }

    @Override
    public LLVMValueRef visitContinue(FContinue fContinue) {
        LLVMBasicBlockRef target = loopJumpPoints.get(fContinue.getLoop().getLoop()).a;
        setDebugLocation(fContinue.getPosition());
        LLVMValueRef res = LLVMBuildBr(builder, target);
        LLVMPositionBuilderAtEnd(builder, target);
        return res;
    }

    @Override
    public LLVMValueRef visitBrackets(FBracketsExpression brackets) {
        return brackets.getInner().accept(this);
    }

    @Override
    public LLVMValueRef visitImplicitCast(FImplicitCast implicitCast) {
        LLVMValueRef toCast = implicitCast.getCastedExpression().accept(this);
        setDebugLocation(implicitCast.getPosition());
        return visitImplicitTypeCast(toCast, implicitCast.getTypeCast());
    }

    private LLVMValueRef visitImplicitTypeCast(LLVMValueRef base, ImplicitTypeCast cast) {
        LLVMTypeRef targetType = module.getLlvmType(cast.getTarget());
        if (cast.isNoOpCast()) {
            if (LLVMTypeOf(base).equals(targetType))
                return base;
            else {
                return LLVMBuildBitCast(builder, base, targetType, "noOpCast");
            }
        }

        if (cast instanceof TypeVariableCast)
            return Utils.cantHappen();
        else if (cast instanceof TypeConversion)
            return visitTypeConversion(base, (TypeConversion) cast);
        else if (cast instanceof TypeParameterCast)
            return Utils.NYI("Type Parameter Cast");
        else if (cast.getBase() == FNull.NULL_TYPE) {
            return getNull(cast.getTarget()); //TODO this is a hack on top of a hack, if this stays I can removed type nulls in frontend, only need null and cast
        }
            return Utils.cantHappen();
    }

    private LLVMValueRef visitTypeConversion(LLVMValueRef base, TypeConversion conversion) {
        assert conversion.getVariance() == Variance.Covariant;

        //TODO the "special handling" in front here is ugly, whenever we make further changes to casts we should reevaluate the construct as a whole and maybe change it in a way that allows both delegate and other conversions to be handled similar again
        if (conversion.getCastType() == TypeConversion.CastType.DELEGATE) {
            FType target = conversion.getInner() == null ? conversion.getTarget() : conversion.getInner().getBase();
            FField field = conversion.getBase().getDirectDelegates().get(target);
            LLVMValueRef addr = LLVMBuildStructGEP(builder, base, module.getFieldIndex(field), "GEP_delegate_" + field.getIdentifier().name);
            LLVMValueRef res = LLVMBuildLoad(builder, addr, "load_delegate_" + field.getIdentifier().name);
            if (conversion.getInner() != null)
                return visitImplicitTypeCast(res, conversion.getInner());
            else
                return res;
        }

        FClass baseClass;
        if (conversion.getInner() != null) {
            base = visitImplicitTypeCast(base, conversion.getInner());
            baseClass = (FClass) conversion.getInner().getTarget();
        } else
            baseClass = conversion.getBase();

        LLVMTypeRef target = module.getLlvmType(conversion.getTarget());

        switch (conversion.getCastType()) {
            case INTEGER_PROMOTION:
                return LLVMBuildSExt(builder, base, target, "cast_int_prom");
            case FLOAT_PROMOTION:
                return LLVMBuildFPExt(builder, base, target, "cast_float_prom");
            case TO_OPTIONAL:
                if (baseClass == FBool.INSTANCE)
                    return LLVMBuildZExt(builder, base, target, "cast_bool_opt");
                else
                    return base;
            case OPTIONAL_TO_BOOL:
                return LLVMBuildICmp(builder, LLVMIntNE, base, getNull(baseClass), "ne");
            default:
                return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitOptElse(FOptElse optElse) {
        //TODO similar to ShortCircuitLogic
        LLVMValueRef optional = optElse.getOptional().accept(this);

        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef startBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef elseBlock = LLVMAppendBasicBlock(currentFunction, "optElse");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "optElseAfter");

        setDebugLocation(optElse.getPosition());
        LLVMValueRef cond = LLVMBuildICmp(builder, LLVMIntNE, optional, getNull(optElse.getOptional().getType()), "checkNull");
        LLVMValueRef then;
        if (optElse.getType() == FBool.INSTANCE) //TODO this is copy paste from explicit cast
            then = LLVMBuildTrunc(builder, optional, module.getLlvmType(FBool.INSTANCE), "bool!");
        else
            then = optional;
        LLVMBuildCondBr(builder, cond, afterBlock, elseBlock);

        LLVMPositionBuilderAtEnd(builder, elseBlock);
        LLVMValueRef elze = optElse.getElse().accept(this);
        setDebugLocation(null);
        LLVMBuildBr(builder, afterBlock);
        elseBlock = LLVMGetInsertBlock(builder);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(optElse.getType()), "optElse_logic");
        LLVMAddIncoming(phi, then, startBlock, 1);
        LLVMAddIncoming(phi, elze, elseBlock, 1);
        return phi;
    }

    @Override
    public LLVMValueRef visitCache(FCacheExpression cache) {
        LLVMValueRef res = cache.getExpression().accept(this);
        LLVMValueRef alloca = createEntryBlockAlloca(cache.getVariable());
        setDebugLocation(cache.getPosition());
        LLVMBuildStore(builder, res, alloca);
        return res;
    }

    private LLVMValueRef predefinedUnary(FFunctionCall functionCall, LLVMValueRef arg) {
        FFunction function = functionCall.getFunction();
        FIdentifier id = function.getIdentifier();
        if (id.equals(UnaryOperator.NOT.identifier))
            return LLVMBuildNot(builder, arg, "not");
        else if (id.equals(UnaryOperator.NEG.identifier))
            if (function.getMemberOf().getType() instanceof FIntN)
                return LLVMBuildNeg(builder, arg, "neg");
            else
                return LLVMBuildFNeg(builder, arg, "neg");

        FType type = function.getSignature().getParameters().get(0).getType();
        if (type instanceof FIntN)
            return predefinedUnaryInt(id, (FIntN) type, arg);
        if (type instanceof FFloat)
            return predefinedUnaryFloat(id, (FFloat) type, arg);
        return Utils.NYI("predefined Unary function call to: " + function);
    }

    private LLVMValueRef predefinedUnaryInt(FIdentifier id, FIntN type, LLVMValueRef arg) {
        if (id.equals(FPredefinedClass.TO_CHAR)) {
            return LLVMBuildTrunc(builder, arg, module.getLlvmType(FIntN._8), "cast_int_dem");
        } else if (id.equals(FPredefinedClass.TO_INT32)) {
            return LLVMBuildTrunc(builder, arg, module.getLlvmType(FIntN._32), "cast_int_dem");
        } else if (id.equals(FPredefinedClass.TO_INT64)) {
            return LLVMBuildTrunc(builder, arg, module.getLlvmType(FIntN._64), "cast_int_dem");
        } else if (id.equals(FPredefinedClass.TO_FLOAT32)) {
            return LLVMBuildSIToFP(builder, arg, module.getLlvmType(FFloat32.INSTANCE), "cast_int_float");
        } else if (id.equals(FPredefinedClass.TO_FLOAT64)) {
            return LLVMBuildSIToFP(builder, arg, module.getLlvmType(FFloat64.INSTANCE), "cast_int_float");
        } else if (id.equals(FIntN.COUNT_LEADING_ZEROS)) {
            LLVMValueRef function = module.getIntIntrinsicFunction("llvm.ctlz", type);
            PointerPointer<LLVMValueRef> args = createPointerPointer(arg, boolLiteral(false));
            return LLVMBuildCall(builder, function , args, 2, "countLeadingZeros");
        } else if (id.equals(FIntN.COUNT_TRAILING_ZEROS)) {
            LLVMValueRef function = module.getIntIntrinsicFunction("llvm.cttz", type);
            PointerPointer<LLVMValueRef> args = createPointerPointer(arg, boolLiteral(false));
            return LLVMBuildCall(builder, function , args, 2, "countTrailingZeros");
        }

        return Utils.NYI("predefined unary Int/Bool operation: " + id);
    }

    private LLVMValueRef predefinedUnaryFloat(FIdentifier id, FFloat type, LLVMValueRef arg) {
        if (id.equals(FPredefinedClass.TO_FLOAT32)) {
            return LLVMBuildFPTrunc(builder, arg, module.getLlvmType(FFloat32.INSTANCE), "cast_float_dem");
        } else if (id.equals(FPredefinedClass.TO_INT32)) {
            return LLVMBuildFPToSI(builder, arg, module.getLlvmType(FIntN._32), "cast_float_int");
        } else if (id.equals(FPredefinedClass.TO_INT64)) {
            return LLVMBuildFPToSI(builder, arg, module.getLlvmType(FIntN._64), "cast_float_int");
        } else if (id.equals(FFloat.RAW_BITS)) {
            return LLVMBuildBitCast(builder, arg, module.getLlvmType(FIntN.getIntN(type.getBits())), "rawFloatBits");
        }

        String floatIntrinsicName = floatIntrinsics.get(id);
        if (floatIntrinsicName != null) {
            LLVMValueRef function = module.getFloatIntrinsicFunction(floatIntrinsicName, type);
            return LLVMBuildCall(builder, function, createPointerPointer(arg), 1, floatIntrinsicName);
        }

        return Utils.NYI("predefined unary Float operation: " + id);
    }

    private LLVMValueRef predefinedBinary(FFunctionCall functionCall, LLVMValueRef left, LLVMValueRef right) {
        FFunction function = functionCall.getFunction();
        FIdentifier id = function.getIdentifier();

        FType type = function.getSignature().getParameters().get(0).getType();
        assert function.getSignature().getParameters().get(1).getType() == type;
        if (type instanceof FIntN || type == FBool.INSTANCE) {
            return predefinedBinaryInt(id, type, left, right);
        } else if (type == FFloat32.INSTANCE || type == FFloat64.INSTANCE) {
            Integer arith = arithFOpMap.get(id);
            if (arith != null)
                return LLVMBuildBinOp(builder, arith, left, right, "arith_" + id.name);
            else
                return LLVMBuildFCmp(builder, cmpFOpMap.get(id), left, right, "cmp_" + id.name);
        } else {
            return Utils.cantHappen();
        }
    }

    private LLVMValueRef predefinedBinaryInt(FIdentifier id, FType type, LLVMValueRef left, LLVMValueRef right) {
        Integer arith = arithOpMap.get(id);
        if (arith != null)
            return LLVMBuildBinOp(builder, arith, left, right, "arith_" + id.name);

        Integer cmp = cmpOpMap.get(id);
        if (cmp != null)
            return LLVMBuildICmp(builder, cmp, left, right, "cmp_" + id.name);

        if (type == FBool.INSTANCE)
            return Utils.NYI("predifined binary Bool operation: " + id);

        FIntN intN = (FIntN) type;

        if (id.equals(FIntN.S_MUL_OVERFLOW)) {
            LLVMValueRef function = module.getIntIntrinsicFunction("llvm.smul.with.overflow", intN);
            PointerPointer<LLVMValueRef> args = createPointerPointer(left, right);
            return LLVMBuildCall(builder, function , args, 2, "smulWithOverflow");
        } else if (id.equals(FIntN.U_MUL_OVERFLOW)) {
            LLVMValueRef function = module.getIntIntrinsicFunction("llvm.umul.with.overflow", intN);
            PointerPointer<LLVMValueRef> args = createPointerPointer(left, right);
            return LLVMBuildCall(builder, function , args, 2, "umulWithOverflow");
        }

        return Utils.NYI("predifined binary Int operation: " + id);
    }

    private LLVMValueRef shortCircuitLogic(List<? extends FExpression> arguments, boolean isAnd) {
        assert arguments.size() == 2;
        LLVMValueRef first = arguments.get(0).accept(this);

        //TODO similar to OptElse
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef startBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef otherBlock = LLVMAppendBasicBlock(currentFunction, "other");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after");

        if (isAnd)
            LLVMBuildCondBr(builder, first, otherBlock, afterBlock);
        else
            LLVMBuildCondBr(builder, first, afterBlock, otherBlock);

        LLVMPositionBuilderAtEnd(builder, otherBlock);
        LLVMValueRef second = arguments.get(1).accept(this);
        LLVMBuildBr(builder, afterBlock);
        otherBlock = LLVMGetInsertBlock(builder);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        setDebugLocation(null);
        LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(FBool.INSTANCE), "ss_logic");
        LLVMAddIncoming(phi, first, startBlock, 1);
        LLVMAddIncoming(phi, second, otherBlock, 1);
        return phi;
    }

    private LLVMValueRef predefinedArray (FFunctionCall functionCall, FArray type, List<LLVMValueRef> args) {
        FFunction function = functionCall.getFunction();
        if (function.isConstructor())
            return buildArrayMalloc(module.getLlvmType(functionCall.getType()), getOnlyElement(args));
        if (function.getIdentifier().equals(Access.ID))
            return visitArrayAccess(args);
        if (function.getIdentifier().equals(FArray.C_ARRAY))
            return arrayGep(args.get(0),indexLiteral(0));
        if (function.getIdentifier().equals(FArray.COPY)) {
            //TODO bounds check
            LLVMValueRef srcAddress = LLVMBuildBitCast(builder, arrayGep(args.get(0), args.get(2)), module.bytePointer, "src");
            LLVMValueRef targetAddress = LLVMBuildBitCast(builder, arrayGep(args.get(1), args.get(3)), module.bytePointer, "target");

            LLVMValueRef bytesToCopyAsPtr = LLVMBuildGEP(builder, LLVMConstNull(LLVMPointerType(module.getLlvmType(type.getBaseType()), 0)), createPointerPointer(args.get(4)), 1, "bytesToCopyPtr");
            LLVMValueRef bytesToCopy = LLVMBuildPtrToInt(builder, bytesToCopyAsPtr, module.getLlvmType(FIntN._32), "bytesToCopy");

            return LLVMBuildCall(builder, module.getMemcopyInstrinsic(), createPointerPointer(targetAddress, srcAddress, bytesToCopy, boolLiteral(false)), 4, "");
        }

        return Utils.NYI(function.headerToString() + " in the backend");
    }

    private LLVMValueRef visitArrayAccess(List<LLVMValueRef> args) {
        LLVMValueRef address = arrayGep(args.get(0), args.get(1));
        return switch (args.size()) {
            case 2 -> LLVMBuildLoad(builder, address, "load_array");
            case 3 -> LLVMBuildStore(builder, args.get(2), address);
            default -> Utils.cantHappen();
        };
    }

    private LLVMValueRef buildArrayMalloc(LLVMTypeRef arrayType, LLVMValueRef sizeRef) {
        LLVMValueRef size = arrayOffsetOf(arrayType, sizeRef);
        LLVMValueRef malloc = LLVMBuildArrayMalloc(builder, module.byteType, size, "arrayMalloc");
        LLVMValueRef arrayRef = LLVMBuildBitCast(builder, malloc, arrayType, "newArray");

        //store size
        LLVMValueRef sizeAddress = LLVMBuildStructGEP(builder, arrayRef, 0, "sizeAddress");
        LLVMBuildStore(builder, sizeRef, sizeAddress);
        return arrayRef;
    }

    private LLVMValueRef predefinedOptional (FFunctionCall functionCall, List<LLVMValueRef> args) { //TODO this is copy & paste from if and call...
        FFunction function = functionCall.getFunction();
        OptionalNamespace optionalNamespace = (OptionalNamespace) function.getMemberOf();
        FOptional optional = optionalNamespace.getType();

        FIdentifier identifier = functionCall.getFunction().getIdentifier();
        if (identifier.equals(UnaryOperator.NOT.identifier)) {
            assert args.size() == 1;
            return LLVMBuildICmp(builder, LLVMIntEQ, args.get(0), getNull(optional), "eq");
        } else if (identifier.equals(FOptional.EXMARK)) {
            assert args.size() == 1;
            //TODO when we have some sort of runtime errors, check before casting and throw errors (and then see if we can avoid checking next)
            //return LLVMBuildICmp(builder, LLVMIntEQ, toCast, module.getNull((FOptional) explicitCast.getCastedExpression().getType()), "check_NPE");
            if (function.getType() == FBool.INSTANCE)
                return LLVMBuildTrunc(builder, args.get(0), module.getLlvmType(FBool.INSTANCE), "bool!");
            else
                return args.get(0);
        }

        FFunction toCall = optionalNamespace.getShimMap().inverse().get(function);
        assert toCall != null;

        LLVMValueRef This = args.get(0);

        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef originalBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef thenBlock = LLVMAppendBasicBlock(currentFunction, "call_nonnull");
        LLVMBasicBlockRef continueBlock = LLVMAppendBasicBlock(currentFunction, "after_call");

        LLVMValueRef If = LLVMBuildICmp(builder, LLVMIntNE, This, getNull(optional), "checkNull");
        LLVMBuildCondBr(builder, If, thenBlock, continueBlock);

        LLVMPositionBuilderAtEnd(builder, thenBlock);
        //call
        //TODO why don't I need to cast first param to non optional?
        LLVMValueRef call = buildCall(toCall, args);
        LLVMBuildBr(builder, continueBlock);

        LLVMPositionBuilderAtEnd(builder, continueBlock);
        if (function.getType() == FTuple.VOID) {
            return null;
        } else {
            LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(function.getType()), "phi_optcall");
            LLVMAddIncoming(phi, call, thenBlock, 1);
            LLVMAddIncoming(phi, getNull(function.getType()), originalBlock, 1); //TODO not sure if I should return This or null, if any of them helps the optimizer? - tuples can make null complex, so prefer this?
            return phi;
        }
    }

    private LLVMValueRef predefinedFunctionCall (FFunctionCall functionCall, List<LLVMValueRef> args) {
        FFunction function = functionCall.getFunction();
        if (function instanceof FieldAccessor) {
            if (function.getMemberOf().getType() instanceof FTuple)
                return visitTupleAccess((FieldAccessor) function, args);
            else
                return visitFieldAccess((FieldAccessor) function, args);
        }
        FType type = function.getMemberOf().getType();
        if (type instanceof FArray) {
            return predefinedArray(functionCall, (FArray) type, args);
        } else if (type instanceof FOptional) {
            return predefinedOptional(functionCall, args);
        } else if (function.getIdentifier().equals(FConstructor.MALLOC_ID)) {
            assert args.isEmpty();
            return LLVMBuildMalloc(builder, LLVMGetElementType(module.getLlvmType(functionCall.getType())), "malloc_" + functionCall.getType().getIdentifier());
        } else if (args.size() == 1) {
            return predefinedUnary(functionCall, args.get(0));
        } else if (args.size() == 2) {
            return predefinedBinary(functionCall, args.get(0), args.get(1));
        } else {
            return Utils.cantHappen();
        }
    }

    private LLVMValueRef visitTupleAccess(FieldAccessor fieldAccessor, List<LLVMValueRef> args) {
        assert fieldAccessor.isInstance();
        assert fieldAccessor.isGetter();
        assert args.size() == 1;

        FField field = fieldAccessor.getField();
        int i = NameGenerator.infixIndex(field.getIdentifier().name);
        return LLVMBuildExtractValue(builder, args.get(0), i, "tupleAccess_" + i);
    }

    private LLVMValueRef visitFieldAccess(FieldAccessor fieldAccessor, List<LLVMValueRef> args) {
        FField field = fieldAccessor.getField();
        LLVMValueRef address;
        if (field.isInstance())
            address = LLVMBuildStructGEP(builder, args.get(0), module.getFieldIndex(field), "GEP_" + field.getIdentifier().name);
        else
            address = LLVMGetNamedGlobal(module.getModule(), getStaticFieldName(field));

        if (fieldAccessor.isGetter())
            return LLVMBuildLoad(builder, address, "load_" + field.getIdentifier().name);
        else
            return LLVMBuildStore(builder, field.isInstance() ? args.get(1) : args.get(0), address);
    }

    @Override
    public LLVMValueRef visitFunctionCall(FFunctionCall functionCall) {
        return visitFunctionCall(functionCall, emptyList());
    }

    public LLVMValueRef visitFunctionCall(FFunctionCall functionCall, Collection<LLVMValueRef> additionalArgs) {
        FFunction function = functionCall.getFunction();
        ImmutableList<FParameter> parameters = functionCall.getSignature().getParameters();
        boolean hasPacking = functionCall.getArgMapping().hasPacking();
        assert !hasPacking || parameters.stream()
                .map(FParameter::getDefaultValueDependencies)
                .allMatch(dep -> dep == null || dep.isEmpty())
                : "can't handle function calls with packing and dependent default args yet, sorry"; //TODO

        //check for short circuit predefined bool ('&&', '||'), because args are parsed differently there
        if (function.isPredefined() && function.getSignature().getParameters().size() == 2) {
            FIdentifier id = function.getIdentifier();
            if (id.equals(BinaryOperator.AND.identifier)) {
                assert !hasPacking && additionalArgs.isEmpty();
                return shortCircuitLogic(functionCall.getArguments(true), true);
            } else if (id.equals(BinaryOperator.OR.identifier)) {
                assert !hasPacking && additionalArgs.isEmpty();
                return shortCircuitLogic(functionCall.getArguments(true), false);
            }
        }

        List<? extends FExpression> arguments = functionCall.getArguments(true);
        LLVMValueRef[] unpreparedArgs = new LLVMValueRef[arguments.size()];
        //given arguments
        for (int i = 0; i < arguments.size(); i++) {
            if (functionCall.isDefaultArg(i))
                continue;
            LLVMValueRef llvmValue = arguments.get(i).accept(this);
            if (!hasPacking) {
                LLVMValueRef old = tempVars.put(parameters.get(i), llvmValue);
                assert old == null;
            }
            unpreparedArgs[i] = llvmValue;
        }
        //default arguments
        for (Integer argIndex : functionCall.computeDefaultArgOrder()) {
            LLVMValueRef llvmValue = arguments.get(argIndex).accept(this);
            tempVars.put(parameters.get(argIndex), llvmValue);
            unpreparedArgs[argIndex] = llvmValue;
        }

        for (FParameter p : parameters)
            tempVars.remove(p);

        setDebugLocation(functionCall.getPosition());
        List<LLVMValueRef> args = prepareArgs(Lists.newArrayList(unpreparedArgs), functionCall.getArgMapping());
        args.addAll(additionalArgs);

        if (function.isPredefined())
            return predefinedFunctionCall(functionCall, args);

        return buildCall(function, args);
    }

    private List<LLVMValueRef> prepareArgs(List<LLVMValueRef> args, ArgMapping argMapping) {
        //unpack
        List<LLVMValueRef> unpacked = argMapping.unpackBase(args, this::unpackTuple);

        //cast
        assert unpacked.size() == argMapping.getCasts().size();
        for (int i = 0; i < unpacked.size(); i++) {
            ImplicitTypeCast cast = argMapping.getCasts().get(i);
            if (cast == null)
                continue;
            unpacked.set(i, visitImplicitTypeCast(unpacked.get(i), cast));
        }

        //repack
        return argMapping.pack(unpacked, this::packTuple);
    }

    private LLVMValueRef buildCall(FFunction toCall, List<LLVMValueRef> args) {
        LLVMValueRef func = LLVMGetNamedFunction(module.getModule(), getFunctionName(toCall));
        assert func != null && !func.isNull() : toCall.getIdentifier() + " could not be found in module";
        String instructionName = toCall.getType() == FTuple.VOID ? "" : "callTmp";
        return LLVMBuildCall(builder, func, createPointerPointer(args), args.size(), instructionName);
    }

    @Override
    public LLVMValueRef visitDynamicFunctionCall(DynamicFunctionCall functionCall) {
        //TODO this doesn't work for predefined functions
        LLVMValueRef function = functionCall.getFunction().accept(this);
        List<LLVMValueRef> args = new ArrayList<>();
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        String instructionName = functionCall.getType() == FTuple.VOID ? "" : "callTmp";
        setDebugLocation(functionCall.getPosition());
        return LLVMBuildCall(builder, function, createPointerPointer(args), args.size(), instructionName);
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
            setDebugLocation(expression.getPosition());
            return LLVMBuildBitCast(builder, res, type, ""); //cast to get rid of the explicit length in the array type to make LLVM happy
        } else if (literal instanceof  FBoolLiteral) {
            return LLVMConstInt(type, ((FBoolLiteral) literal).value ? TRUE : FALSE, FALSE);
        } else if (literal instanceof FNull) {
            if (literal == FNull.UNTYPED)
                return LLVMConstPointerNull(type); //TODO
            return getNull(literal.getType());
        } else {
            return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitVariable(FVariableExpression expression) {
        LLVMValueRef address = localVars.get(expression.getVariable());
        if (address == null) {
            assert tempVars.containsKey(expression.getVariable());
            return tempVars.get(expression.getVariable());
        }
        return switch (expression.getAccessType()) {
            case LOAD -> {
                setDebugLocation(expression.getPosition());
                yield LLVMBuildLoad(builder, address, expression.getVariable().getIdentifier().name);
            }
            case STORE -> address;
        };
    }

    @Override
    public LLVMValueRef visitNamespaceExpression(FNamespaceExpression expression) {
        return module.getTypeInfo(expression.getNamespace().getType());
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
        PointerPointer<LLVMValueRef> indices = createPointerPointer(indexLiteral(0), indexLiteral(1), index);
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

    private List<LLVMValueRef> unpackTuple(LLVMValueRef tuple) {
        int size = LLVMCountStructElementTypes(LLVMTypeOf(tuple));
        ArrayList<LLVMValueRef> res = new ArrayList<>(size);
        for (int i = 0; i < size; i++)
            res.add(LLVMBuildExtractValue(builder, tuple, i, "unpack_" + i));
        return res;
    }

    private LLVMValueRef packTuple(List<LLVMValueRef> values) {
        assert values.size() > 1;

        PointerPointer<LLVMTypeRef> types = createPointerPointer(values, LLVM::LLVMTypeOf);
        LLVMTypeRef structType = LLVMStructTypeInContext(module.getContext(), types, values.size(), FALSE);

        LLVMValueRef agg = LLVMGetUndef(structType);
        for (int i = 0; i < values.size(); i++)
            agg = LLVMBuildInsertValue(builder, agg, values.get(i), i, "pack_" + i);
        return agg;
    }


    private LLVMValueRef getNull(FType fOptional) { //note: this function would belong into module if it weren't for tuples
        if (fOptional instanceof FTuple) {
            List<FType> fTypes = ((FTuple) fOptional).getTypes();
            List<LLVMValueRef> llvmTypes = new ArrayList<>(fTypes.size());
            for (FType type : fTypes) {
                llvmTypes.add(getNull(type));
            }
            return packTuple(llvmTypes);
        }

        assert fOptional instanceof FOptional;
        FType base = ((FOptional) fOptional).getBaseType();
        if (base == FBool.INSTANCE) {
            return LLVMConstInt(LLVMIntTypeInContext(module.getContext(), 2), 2, FALSE);
        } else if (base instanceof FIntN) {
            return LLVMConstInt(module.getLlvmType(base), ((FIntN) base).minValue().subtract(BigInteger.ONE).longValue(), FALSE);
        } else if (base instanceof FFloat32 || base instanceof FFloat64) {
            return Utils.NYI("null literal for floating point types");
        } else if (base instanceof FTuple) {
            return Utils.cantHappen();
        } else {
            return LLVMConstPointerNull(module.getLlvmType(base));
        }
    }

    private LLVMMetadataRef createFunctionDebugInfo(FFunction function, LLVMValueRef llvmFunction) {
        if (diBuilder == null || function.getLocation() == null)
            return null;
        LLVMMetadataRef fileScope = createFileScope(function.getLocation());
        String name = function.getIdentifier().name;
        LLVMMetadataRef functionType = LLVMDIBuilderCreateSubroutineType(diBuilder, fileScope, (PointerPointer) null, 0, 0);
        @SuppressWarnings("OptionalGetWithoutIsPresent")
        LLVMMetadataRef functionMetadata = LLVMDIBuilderCreateFunction(diBuilder, fileScope, name, name.length(), "", 0, fileScope, function.getLocation().getPoint().getLineFrom(), functionType, TRUE, TRUE, function.getBody().get().getPosition().getLineFrom(), 0, FALSE);
        LLVMSetSubprogram(llvmFunction, functionMetadata);
        return functionMetadata;
    }

    private LLVMMetadataRef createFileScope(Location location) {
        String directory = location.getFile().getRoot().toString();
        String fileName = location.getFile().toString().substring(directory.length());
        return LLVMDIBuilderCreateFile(diBuilder, fileName, fileName.length(), directory, directory.length());
    }

    private void setDebugLocation(Position position) {
        if (diBuilder == null || debugScope == null)
            return;

        if (position != null ) {
            LLVMMetadataRef llvmMetadataRef = LLVMDIBuilderCreateDebugLocation(module.getContext(), position.getLineFrom(), position.getColumnFrom(), debugScope, null);
            LLVMSetCurrentDebugLocation2(builder, llvmMetadataRef);
        } else {
            LLVMSetCurrentDebugLocation2(builder, null);
        }
    }

}
