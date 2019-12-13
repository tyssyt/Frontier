package tys.frontier.backend.llvm;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import org.bytedeco.javacpp.LLVM;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FField;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.FParameter;
import tys.frontier.code.expression.*;
import tys.frontier.code.expression.cast.*;
import tys.frontier.code.function.FConstructor;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.operator.BinaryOperator;
import tys.frontier.code.function.operator.UnaryOperator;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.typeInference.Variance;
import tys.frontier.code.visitor.ClassWalker;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;
import tys.frontier.util.expressionListToTypeListMapping.ArgMapping;

import java.math.BigInteger;
import java.util.*;

import static org.bytedeco.javacpp.LLVM.*;
import static tys.frontier.backend.llvm.LLVMUtil.*;
import static tys.frontier.code.function.operator.BinaryOperator.*;

class LLVMTransformer implements
        AutoCloseable,
        ClassWalker<LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef, LLVMValueRef> {

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    private static final ImmutableMap<FFunctionIdentifier, Integer> arithOpMap = ImmutableMap.<FFunctionIdentifier, Integer>builder()
            .put(PLUS.identifier, LLVMAdd)
            .put(MINUS.identifier, LLVMSub)
            .put(TIMES.identifier, LLVMMul)
            .put(DIVIDED.identifier, LLVMSDiv)
            .put(MODULO.identifier, LLVMSRem)
            .put(AAND.identifier, LLVMAnd)
            .put(AOR.identifier, LLVMOr)
            .put(XOR.identifier, LLVMXor)
            .build();
    private static final ImmutableMap<FFunctionIdentifier, Integer> cmpOpMap = ImmutableMap.<FFunctionIdentifier, Integer>builder()
            .put(EQUALS.identifier, LLVMIntEQ)
            .put(EQUALS_ID.identifier, LLVMIntEQ)
            .put(NOT_EQUALS.identifier, LLVMIntNE)
            .put(NOT_EQUALS_ID.identifier, LLVMIntNE)
            .put(LESS.identifier, LLVMIntSLT)
            .put(GREATER.identifier, LLVMIntSGT)
            .put(LESS_EQUAL.identifier, LLVMIntSLE)
            .put(GREATER_EQUAL.identifier, LLVMIntSGE)
            .build();
    private static final ImmutableMap<FFunctionIdentifier, Integer> arithFOpMap = ImmutableMap.<FFunctionIdentifier, Integer>builder()
            .put(PLUS.identifier, LLVMFAdd)
            .put(MINUS.identifier, LLVMFSub)
            .put(TIMES.identifier, LLVMFMul)
            .put(DIVIDED.identifier, LLVMFDiv)
            .put(MODULO.identifier, LLVMFRem)
            .build();
    private static final ImmutableMap<FFunctionIdentifier, Integer> cmpFOpMap = ImmutableMap.<FFunctionIdentifier, Integer>builder()
            .put(EQUALS.identifier, LLVMRealOEQ)
            .put(EQUALS_ID.identifier, LLVMRealOEQ)
            .put(NOT_EQUALS.identifier, LLVMRealONE)
            .put(NOT_EQUALS_ID.identifier, LLVMRealONE)
            .put(LESS.identifier, LLVMRealOLT)
            .put(GREATER.identifier, LLVMRealOGT)
            .put(LESS_EQUAL.identifier, LLVMRealOLE)
            .put(GREATER_EQUAL.identifier, LLVMRealOGE)
            .build();

    private LLVMModule module;
    private LLVMBuilderRef builder;
    private LLVMBuilderRef entryBlockAllocaBuilder;
    private Map<FLocalVariable, LLVMValueRef> localVars = new HashMap<>();
    private Map<FLoop, Pair<LLVMBasicBlockRef, LLVMBasicBlockRef>> loopJumpPoints = new HashMap<>();

    private final LLVMTypeRef indexType;

    private LLVMValueRef sfInit;
    private LLVMValueRef cStringToFString;


    public LLVMTransformer(LLVMModule module) {
        this.module = module;
        this.builder = module.createBuilder();
        this.entryBlockAllocaBuilder = module.createBuilder();
        indexType = module.getLlvmType(FIntN._32);

        sfInit = LLVMAddFunction(module.getModule(), "sf.init", LLVMFunctionType(module.getLlvmType(FTuple.VOID), (PointerPointer<LLVMTypeRef>) null, 0, FALSE));
        LLVMAppendBasicBlock(sfInit, "entry");
        cStringToFString = createCStringToFString(); //TODO this is not the ideal place to create this function
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


    public void generateMain(FFunction entryPoint) {
        PointerPointer<LLVMTypeRef> argTypes = LLVMUtil.createPointerPointer(indexType,
                LLVMPointerType(LLVMPointerType(module.getLlvmType(FIntN._8), 0), 0)
        );


        LLVMTypeRef functionType = LLVMFunctionType(indexType, argTypes, 2, FALSE);

        LLVMValueRef function = LLVMAddFunction(module.getModule(), "main", functionType);
        LLVMBasicBlockRef allocaBlock = LLVMAppendBasicBlock(function, "alloca");
        LLVMPositionBuilderAtEnd(entryBlockAllocaBuilder, allocaBlock);
        LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlock(function, "entry");
        LLVMPositionBuilderAtEnd(builder, entryBlock);
        LLVMBuildCall(builder, sfInit, null, 0, "");
        LLVMValueRef userMain = LLVMGetNamedFunction(module.getModule(), getFunctionName(entryPoint));

        //call entry Point
        if (entryPoint.getParams().isEmpty()) {
            LLVMBuildCall(builder, userMain, null, 0, "");
        } else {
            //convert input to Frontier String
            LLVMValueRef args = convertArgs(function, LLVMGetParam(function, 0), LLVMGetParam(function, 1));
            LLVMBuildCall(builder, userMain, LLVMUtil.createPointerPointer(args), 1, "");
        }
        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, FALSE));
        LLVMBuildBr(entryBlockAllocaBuilder, entryBlock);

        //finish sfInit
        LLVMPositionBuilderAtEnd(builder, LLVMGetEntryBasicBlock(sfInit));
        LLVMBuildRetVoid(builder);
    }

    private LLVMValueRef convertArgs(LLVMValueRef function, LLVMValueRef argi, LLVMValueRef argv) {
        LLVMTypeRef fStringType = module.getLlvmType(FArray.getArrayFrom(FStringLiteral.TYPE));

        LLVMBasicBlockRef copyBlock = LLVMAppendBasicBlock(function, "copy");
        LLVMBasicBlockRef copy2Block = LLVMAppendBasicBlock(function, "copy2");
        LLVMBasicBlockRef endBlock = LLVMAppendBasicBlock(function, "end");

        LLVMValueRef i = LLVMBuildAlloca(entryBlockAllocaBuilder, indexType, "alloc_i");

        //TODO this is a alloca copy of arraMalloc
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
        PointerPointer<LLVMValueRef> indices = LLVMUtil.createPointerPointer(load_i);
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
        PointerPointer<LLVMValueRef> indices = LLVMUtil.createPointerPointer(inc);
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
        indices = LLVMUtil.createPointerPointer(load_i);
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
        if (function.getType() == FTuple.VOID && !function.getBody().get().redirectsControlFlow().isPresent())
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
        boolean hasContinue = !fIf.redirectsControlFlow().isPresent();

        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef ifBlock = LLVMAppendBasicBlock(currentFunction, "if");
        LLVMBasicBlockRef thenBlock = LLVMAppendBasicBlock(currentFunction, "then");
        LLVMBasicBlockRef elseBlock = hasElse ? LLVMAppendBasicBlock(currentFunction, "else") : null;
        LLVMBasicBlockRef continueBlock = hasContinue ? LLVMAppendBasicBlock(currentFunction, "after_if") : null;

        LLVMBuildBr(builder, ifBlock);

        LLVMPositionBuilderAtEnd(builder, ifBlock);
        LLVMValueRef condition = fIf.getCondition().accept(this);
        LLVMBuildCondBr(builder, condition, thenBlock, hasElse ? elseBlock : continueBlock);

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

        if (hasContinue)
            LLVMPositionBuilderAtEnd(builder, continueBlock);
        return null;
    }

    @Override
    public LLVMValueRef visitReturn(FReturn fReturn) {
        List<LLVMValueRef> values = new ArrayList<>(fReturn.getExpressions().size());
        for (FExpression arg : fReturn.getExpressions())
            values.add(arg.accept(this));
        FType type = fReturn.getFunction().getType();
        values = prepareArgs(values,
                type == FTuple.VOID ? Collections.emptyList() : Collections.singletonList(type),
                fReturn.getArgMapping());

        switch (values.size()) {
            case 0:
                return LLVMBuildRetVoid(builder);
            case 1:
                return LLVMBuildRet(builder, Iterables.getOnlyElement(values));
            default:
                return LLVMBuildAggregateRet(builder, LLVMUtil.createPointerPointer(values), values.size());
        }
    }

    @Override
    public LLVMValueRef visitVarAssignment(FVarAssignment assignment) {
        if (assignment.getOperator() != FVarAssignment.Operator.ASSIGN) {
            Utils.NYI("operator " + assignment.getOperator());
        }

        List<LLVMValueRef> values = new ArrayList<>();
        for (FExpression arg : assignment.getValues())
            values.add(arg.accept(this));
        values = prepareArgs(values, Utils.typesFromExpressionList(assignment.getVariables()), assignment.getArgMapping());

        for (Pair<FVariableExpression, LLVMValueRef> pair : Utils.zip(assignment.getVariables(), values)) {
            FVariableExpression variable = pair.a;
            if (variable instanceof FVarDeclaration) {
                createEntryBlockAlloca(((FVarDeclaration) variable).getVariable());
            }
            LLVMBuildStore(builder, pair.b, variable.accept(this));
        }
        return null;
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
        return visitImplicitTypeCast(toCast, implicitCast.getTypeCast());
    }

    private LLVMValueRef visitImplicitTypeCast(LLVMValueRef base, ImplicitTypeCast cast) {
        LLVMTypeRef targetType = module.getLlvmType(cast.getTarget());
        if (cast.isNoOpCast()) {
            if (LLVMTypeOf(base).equals(targetType))
                return base;
            else
                return LLVMBuildBitCast(builder, base, targetType, "noOpCast");
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

        LLVMValueRef cond = LLVMBuildICmp(builder, LLVMIntNE, optional, getNull(optElse.getOptional().getType()), "checkNull");
        LLVMValueRef then;
        if (optElse.getType() == FBool.INSTANCE) //TODO this is copy paste from explicit cast
            then = LLVMBuildTrunc(builder, optional, module.getLlvmType(FBool.INSTANCE), "bool!");
        else
            then = optional;
        LLVMValueRef elze = optElse.getElse().accept(this);
        return LLVMBuildSelect(builder, cond, then, elze, "ifExpr");
    }

    private LLVMValueRef predefinedUnary(FFunctionCall functionCall, List<LLVMValueRef> args) {
        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        LLVMValueRef arg = Iterables.getOnlyElement(args);
        if (id.equals(UnaryOperator.NOT.identifier))
            return LLVMBuildNot(builder, arg, "not");
        else if (id.equals(UnaryOperator.NEG.identifier))
            return LLVMBuildNeg(builder, arg, "neg");
        else if (id.equals(UnaryOperator.INC.identifier))
            return incDec(arg, LLVMAdd);
        else if (id.equals(UnaryOperator.DEC.identifier))
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

    private LLVMValueRef predefinedBinary(FFunctionCall functionCall, List<LLVMValueRef> args) {
        assert args.size() == 2;
        LLVMValueRef left = args.get(0);
        LLVMValueRef right = args.get(1);
        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();

        if (id.equals(BinaryOperator.AND.identifier))
            return shortCircuitLogic(left, right, true);
        else if (id.equals(BinaryOperator.OR.identifier))
            return shortCircuitLogic(left, right, false);

        if (functionCall.getFunction().getMemberOf() instanceof FIntN || functionCall.getFunction().getMemberOf() == FBool.INSTANCE) {
            Integer arith = arithOpMap.get(id);
            if (arith != null)
                return LLVMBuildBinOp(builder, arith, left, right, "arith_" + id.name);
            else
                return LLVMBuildICmp(builder, cmpOpMap.get(id), left, right, "cmp_" + id.name);
        } else {
            Integer arith = arithFOpMap.get(id);
            if (arith != null)
                return LLVMBuildBinOp(builder, arith, left, right, "arith_" + id.name);
            else
                return LLVMBuildFCmp(builder, cmpFOpMap.get(id), left, right, "cmp_" + id.name);
        }
    }

    private LLVMValueRef shortCircuitLogic(LLVMValueRef first, LLVMValueRef second, boolean isAnd) {
        LLVMValueRef currentFunction = getCurrentFunction();
        LLVMBasicBlockRef startBlock = LLVMGetInsertBlock(builder);
        LLVMBasicBlockRef otherBlock = LLVMAppendBasicBlock(currentFunction, "other");
        LLVMBasicBlockRef afterBlock = LLVMAppendBasicBlock(currentFunction, "after");

        if (isAnd)
            LLVMBuildCondBr(builder, first, otherBlock, afterBlock);
        else
            LLVMBuildCondBr(builder, first, afterBlock, otherBlock);

        LLVMPositionBuilderAtEnd(builder, otherBlock);
        LLVMBuildBr(builder, afterBlock);

        LLVMPositionBuilderAtEnd(builder, afterBlock);
        LLVMValueRef phi = LLVMBuildPhi(builder, module.getLlvmType(FBool.INSTANCE), "ss_logic");
        LLVMAddIncoming(phi, first, startBlock, 1);
        LLVMAddIncoming(phi, second, otherBlock, 1);
        return phi;
    }

    private LLVMValueRef predefinedArray (FFunctionCall functionCall, List<LLVMValueRef> args) {
        FFunction function = functionCall.getFunction();
        if (function.isConstructor()) {
            return buildArrayMalloc(module.getLlvmType(functionCall.getType()), Iterables.getOnlyElement(args));
        } else
            return Utils.NYI(function.headerToString() + " in the backend");
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
        FOptional optional = (FOptional) function.getMemberOf();
        FFunction toCall = optional.getShimMap().inverse().get(function);
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
        if (function.getMemberOf() instanceof FArray) {
            return predefinedArray(functionCall, args);
        } else if (function.getMemberOf() instanceof FOptional) {
            return predefinedOptional(functionCall, args);
        } else if (function.getIdentifier().equals(FConstructor.MALLOC_ID)) {
            assert args.isEmpty();
            return LLVMBuildMalloc(builder, LLVMGetElementType(module.getLlvmType(functionCall.getType())), "malloc_" + functionCall.getType().getIdentifier());
        } else if (args.size() == 1) {
            return predefinedUnary(functionCall, args);
        } else if (args.size() == 2) {
            return predefinedBinary(functionCall, args);
        } else {
            return Utils.cantHappen();
        }
    }

    @Override
    public LLVMValueRef visitFunctionCall(FFunctionCall functionCall) {
        //given arguments
        List<LLVMValueRef> args = new ArrayList<>();
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        args = prepareArgs(args, Utils.typesFromExpressionList(functionCall.getFunction().getParams()), functionCall.getArgMapping());

        FFunction function = functionCall.getFunction();
        if (function.isPredefined())
            return predefinedFunctionCall(functionCall, args);

        return buildCall(function, args);
    }

    private List<LLVMValueRef> prepareArgs(List<LLVMValueRef> args, List<FType> target, ArgMapping argMapping) { //TODO prolly store the info we get via target in argMapping
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
            if (literal == FNull.UNTYPED)
                return LLVMConstPointerNull(type); //TODO
            return getNull(literal.getType());
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
        PointerPointer<LLVMValueRef> indices = LLVMUtil.createPointerPointer(indexLiteral(0), indexLiteral(1), index);
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

        PointerPointer<LLVMTypeRef> types = LLVMUtil.createPointerPointer(values, LLVM::LLVMTypeOf);
        LLVMTypeRef structType = LLVMStructTypeInContext(module.getContext(), types, values.size(), FALSE);

        LLVMValueRef agg = LLVMGetUndef(structType);
        for (int i = 0; i < values.size(); i++)
            agg = LLVMBuildInsertValue(builder, agg, values.get(i), i, "pack_" + i);
        return agg;
    }


    LLVMValueRef getNull(FType fOptional) { //note: this function would belong into module if it weren't for tuples
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
}
