package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.LLVMContextRef;
import org.bytedeco.llvm.LLVM.LLVMMemoryBufferRef;
import org.bytedeco.llvm.LLVM.LLVMModuleRef;
import tys.frontier.code.FField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;

import static org.bytedeco.llvm.global.LLVM.*;

public class LLVMUtil {

    private LLVMUtil() {} //no instances

    //TODO this is kind of a temporary solution, but I will have to handle LLVM naming more when I start to allow renaming etc. anyways
    private static Map<FFunction, String> uniqueFunctionNameCache = new HashMap<>();

    public static <P extends Pointer> PointerPointer<P> createPointerPointer (P p) {
        PointerPointer<P> res = new PointerPointer<>(1);
        res.put(0, p);
        return res;
    }

    @SafeVarargs
    public static <P extends Pointer> PointerPointer<P> createPointerPointer (P... list) {
        return new PointerPointer<>(list);
    }

    public static <P extends Pointer> PointerPointer<P> createPointerPointer (Collection<P> collection) {
        PointerPointer<P> res = new PointerPointer<>(collection.size());
        Iterator<P> iterator = collection.iterator();
        for (int i=0; i<collection.size(); i++)
            res.put(i, iterator.next());
        return res;
    }

    public static <P extends Pointer, L> PointerPointer<P> createPointerPointer (Collection<L> collection, Function<L,P> function) {
        PointerPointer<P> res = new PointerPointer<>(collection.size());
        Iterator<L> iterator = collection.iterator();
        for (int i=0; i<collection.size(); i++)
            res.put(i, function.apply(iterator.next()));
        return res;
    }

    public static String getClassName(FClass clazz) {
        if (clazz.getNamespace().getNative() != null)
            return clazz.getNamespace().getNative().getValue().orElse(clazz.getIdentifier().name);
        return "class." + clazz.getIdentifier().name;
    }

    public static String getStaticFieldName(FField field) {
        return "sf." + field.getNamespace().getIdentifier().name + '.' + field.getIdentifier().name;
    }

    public static String getFunctionName(FFunction function) {
        if (function.getNative() != null)
            return function.getNative().getValue().orElse(function.getIdentifier().name);
        String uniqueName = uniqueFunctionNameCache.get(function);
        if (uniqueName == null) {
            Map<FFunction, String> newNames = ((DefaultNamespace) function.getMemberOf()).computeUniqueFunctionNames();
            uniqueFunctionNameCache.putAll(newNames);
            uniqueName = newNames.get(function);
        }
        return "fun." + function.getMemberOf().getIdentifier().name + '.' + uniqueName;
    }

    public static String getConstantStringName(String s) {
        return "const.String." + s;
    }

    public static String getTypeInfoName(FType clazz) {
        return "typeInfo." + clazz.getIdentifier().name;
    }

    public static String getTypeInfoFieldsName(FType clazz) {
        return "typeInfo.fields." + clazz.getIdentifier().name;
    }

    public static String getFieldInfoName(FField field) {
        return "fieldInfo." + field.getNamespace().getIdentifier().name + '.' + field.getIdentifier().name;
    }

    public static LLVMModuleRef loadModuleFromBitcode(LLVMContextRef contextRef, String path) {
        BytePointer error = new BytePointer();
        var outMemBuf = new PointerPointer<LLVMMemoryBufferRef>(1);
        int errorCode = LLVMCreateMemoryBufferWithContentsOfFile(path, outMemBuf, error);
        if (errorCode != 0) {
            String errorString = error.getString();
            LLVMDisposeMessage(error);
            return Utils.handleError(errorString);
        }
        var memoryBuffer = outMemBuf.get(LLVMMemoryBufferRef.class, 0);

        var outModule = new PointerPointer<LLVMModuleRef>(1);
        errorCode = LLVMParseBitcodeInContext2(contextRef, memoryBuffer, outModule);
        if (errorCode != 0) {
            return Utils.handleError("unable to Parse: " + path);
        }

        LLVMDisposeMemoryBuffer(memoryBuffer);
        return outModule.get(LLVMModuleRef.class, 0);
    }

}
