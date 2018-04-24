package tys.frontier.backend.llvm;

import com.koloboke.collect.map.hash.HashObjIntMap;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;
import tys.frontier.code.FLocalVariable;
import tys.frontier.code.expression.*;
import tys.frontier.code.identifier.FFunctionIdentifier;
import tys.frontier.code.literal.*;
import tys.frontier.code.predefinedClasses.*;
import tys.frontier.code.statement.*;
import tys.frontier.code.statement.loop.*;
import tys.frontier.code.visitor.ClassWalker;

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

    private LLVMModuleRef module;
    private LLVMBuilderRef builder;
    private LLVMBuilderRef entryBlockAllocaBuilder;
    private Map<FClass, LLVMTypeRef> llvmTypes;
    private HashObjIntMap<FField> fieldIndices;
    private Map<FField, LLVMValueRef> fields = new HashMap<>();
    private Map<FLocalVariable, LLVMValueRef> localVars = new HashMap<>();

    public LLVMTransformer(LLVMModule module) {
        LLVMContextRef context = module.getContext();
        this.module = module.getModule();
        this.llvmTypes = module.getLlvmTypes();
        this.fieldIndices = module.getFieldIndices();
        this.builder = LLVMCreateBuilderInContext(context);
        this.entryBlockAllocaBuilder = LLVMCreateBuilderInContext(context);
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
        LLVMValueRef res = LLVMBuildAlloca(entryBlockAllocaBuilder, llvmTypes.get(variable.getType()), variable.getIdentifier().name);
        LLVMValueRef old = localVars.put(variable, res);
        if (old != null)
            throw new RuntimeException("variable was already declared, this should not happen"); //TODO error handling
        return res;
    }

    @Override
    public LLVMValueRef visitFunction(FFunction function) {
        LLVMValueRef res = LLVMGetNamedFunction(module, getFunctionName(function));
        if (res.isNull()) {
            throw new RuntimeException("no Prototype defined for: " + function.getIdentifier().name); //TODO when we have proper error handling, this needs tp be handled properly
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
        List<FLocalVariable> fParams = function.getParams();
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
            throw new RuntimeException("Operator " + assignment.getOperator() + " not supported yet"); //TODO
        }
        LLVMValueRef value = assignment.getValue().accept(this);
        LLVMValueRef variableAddress = assignment.getVariableExpression().accept(this);
        return LLVMBuildStore(builder, value, variableAddress);
    }

    @Override
    public LLVMValueRef visitWhile(FWhile fWhile) {
        throw new RuntimeException("no loops, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitFor(FFor fFor) {
        throw new RuntimeException("no loops, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitForEach(FForEach forEach) {
        throw new RuntimeException("no loops, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitEmpty(FEmptyStatement statement) {
        return null;
    }

    @Override
    public LLVMValueRef visitBreak(FBreak fBreak) {
        throw new RuntimeException("no loops, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitContinue(FContinue fContinue) {
        throw new RuntimeException("no loops, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitArrayAccess(FArrayAccess arrayAccess) {
        throw new RuntimeException("no arrys, sorry"); //TODO
    }

    @Override
    public LLVMValueRef visitBrackets(FBracketsExpression brackets) {
        return brackets.getInner().accept(this);
    }

    private LLVMValueRef predefinedUnary(FFunctionCall functionCall) {
        LLVMValueRef arg;
        if (functionCall.getFunction().isStatic()) {
            arg = functionCall.getArguments().get(0).accept(this);
        } else {
            arg = functionCall.getObject().accept(this);
        }

        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        if (id == FFunctionIdentifier.NOT) {
            return LLVMBuildNot(builder, arg, "nottmp");
        } else if (id == FFunctionIdentifier.MINUS) {
            return LLVMBuildNeg(builder, arg, "negtmp");
        } else if (id == FFunctionIdentifier.HASHCODE) {
            throw new RuntimeException("fuck Hashcode, I can't even parse that yet"); //TODO
        } else {
            throw new RuntimeException("unknown predefined unary operator: " + functionCall.getFunction().headerToString());
        }
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
        //TODO promotion? propably done in some other part of frontier though since the frontend needs to understand it as well

        FFunctionIdentifier id = functionCall.getFunction().getIdentifier();
        if (id == FFunctionIdentifier.PLUS) {
            return LLVMBuildAdd(builder, left, right, "addtmp");
        } else if (id == FFunctionIdentifier.MINUS) {
            return LLVMBuildSub(builder, left, right, "subtmp");
        } else if (id == FFunctionIdentifier.TIMES) {
            return LLVMBuildMul(builder, left, right, "multmp");
        } else if (id == FFunctionIdentifier.DIVIDED) {
            return LLVMBuildSDiv(builder, left, right, "sdivtmp");
        } else if (id == FFunctionIdentifier.MODULO) {
            return LLVMBuildSRem(builder, left, right, "sremtmp");
        } else if (id == FFunctionIdentifier.AND) {
            return LLVMBuildAnd(builder, left, right, "andtmp");
        } else if (id == FFunctionIdentifier.OR) {
            return LLVMBuildOr(builder, left, right, "ortmp");
        } else if (id == FFunctionIdentifier.XOR) {
            return LLVMBuildXor(builder, left, right, "xortmp");
        } else if (id == FFunctionIdentifier.SMALLER) {
            return LLVMBuildICmp(builder, LLVMIntSLT, left, right, "slttmp");
        } else if (id == FFunctionIdentifier.GREATER) {
            return LLVMBuildICmp(builder, LLVMIntSGT, left, right, "sgttmp");
        } else if (id == FFunctionIdentifier.SMALLER_EQUAL) {
            return LLVMBuildICmp(builder, LLVMIntSLE, left, right, "sletmp");
        } else if (id == FFunctionIdentifier.GREATER_EQUAL) {
            return LLVMBuildICmp(builder, LLVMIntSGE, left, right, "sgetmp");
        } else if (id == FFunctionIdentifier.EQUALS) {
            return LLVMBuildICmp(builder, LLVMIntEQ, left, right, "eqtmp");
        } else {
            throw new RuntimeException("unknown predefined binary operator: " + functionCall.getFunction().headerToString());
        }
    }

    private LLVMValueRef predefinedFunctionCall (FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function instanceof FPredefinedOperator.Unary) {
            return predefinedUnary(functionCall);
        } else if (function instanceof FPredefinedOperator.Binary) {
            return predefinedBinary(functionCall);
        } else if (function.getClazz() instanceof FArray) {
            throw new RuntimeException("yeah predef sucks and i need to clean up functions first :("); //TODO
        } else {
            throw new RuntimeException("unknown predefined function: " + function.headerToString());
        }
    }

    @Override
    public LLVMValueRef visitFunctionCall(FFunctionCall functionCall) {
        FFunction function = functionCall.getFunction();
        if (function.isPredefined())
            return predefinedFunctionCall(functionCall);

        LLVMValueRef func = LLVMGetNamedFunction(module, getFunctionName(function));
        int size = functionCall.getArguments().size();
        List<LLVMValueRef> args = new ArrayList<>();
        if (!function.isStatic()) {
            size++;
            args.add(functionCall.getObject().accept(this));
        }
        for (FExpression arg : functionCall.getArguments())
            args.add(arg.accept(this));
        String instructionName = function.getType() == FVoid.INSTANCE ? "" : "callTmp";
        return LLVMBuildCall(builder, func, createPointerPointer(args), size, instructionName);
    }

    @Override
    public LLVMValueRef visitFieldAccess(FFieldAccess fieldAccess) {
        FField field = fieldAccess.getField();
        LLVMValueRef address;
        if (field.isStatic()) {
            address = LLVMGetNamedGlobal(module, getStaticFieldName(field));
        } else {
            LLVMValueRef object = fieldAccess.getObject().accept(this);
            address = LLVMBuildStructGEP(builder, object, fieldIndices.getInt(field), "GEP_" + field.getIdentifier().name);
        }
        switch (fieldAccess.getAccessType()) {
            case LOAD:
                return LLVMBuildLoad(builder, address, "load_" + field.getIdentifier().name);
            case STORE:
                return address;
            default:
                throw new RuntimeException();
        }
    }

    @Override
    public LLVMValueRef visitLiteral(FLiteralExpression expression) {
        FLiteral literal = expression.getLiteral();
        if (literal instanceof FIntLiteral) {
            throw new RuntimeException("no ints yet, please specify size"); //TODO
        } else if (literal instanceof FInt32Literal) {
            return LLVMConstInt(llvmTypes.get(FInt32.INSTANCE), ((FInt32Literal)literal).value, TRUE);
        } else if (literal instanceof FInt64Literal) {
            return LLVMConstInt(llvmTypes.get(FInt64.INSTANCE), ((FInt64Literal) literal).value, TRUE);
        } else if (literal instanceof FFloat32Literal) {
            return LLVMConstRealOfString(llvmTypes.get(FFloat32.INSTANCE), ((FFloat32Literal)literal).originalString);
        } else if (literal instanceof FFloat64Literal) {
            return LLVMConstRealOfString(llvmTypes.get(FFloat64.INSTANCE), ((FFloat64Literal)literal).originalString);
        } else if (literal == FBoolLiteral.TRUE) {
            return LLVMConstInt(llvmTypes.get(FBool.INSTANCE), TRUE, FALSE);
        } if (literal == FBoolLiteral.FALSE) {
            return LLVMConstInt(llvmTypes.get(FBool.INSTANCE), FALSE, FALSE);
        } else if (literal == FNull.INSTANCE) {
            //return LLVMConstNull()
            //return LLVMConstPointerNull()
            throw new RuntimeException("no null, im sorry"); //TODO
        } else {
            throw new RuntimeException("unknown literal: " + literal);
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
                throw new RuntimeException();
        }
    }
}
