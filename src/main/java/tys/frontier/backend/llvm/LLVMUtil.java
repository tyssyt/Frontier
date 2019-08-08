package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.util.Utils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class LLVMUtil {

    private LLVMUtil() {} //no instances

    //TODO this is kind of a temporary solution, but I will have to handle LLVM naming more when I start to allow renaming etc. anyways
    private static Map<FFunction, String> uniqueFunctionNameCache = new HashMap<>();

    @SafeVarargs
    public static <P extends Pointer> PointerPointer<P> createPointerPointer (P... list) {
        return createPointerPointer(Arrays.asList(list));
    }

    public static <P extends Pointer> PointerPointer<P> createPointerPointer (List<P> list) {
        PointerPointer<P> res = new PointerPointer<>(list.size());
        for (int i=0; i<list.size(); i++)
            res.put(i, list.get(i));
        return res;
    }

    public static <P extends Pointer, L> PointerPointer<P> createPointerPointer (List<L> list, Function<L,P> function) {
        PointerPointer<P> res = new PointerPointer<>(list.size());
        for (int i=0; i<list.size(); i++)
            res.put(i, function.apply(list.get(i)));
        return res;
    }

    public static String getClassName(FType clazz) {
        return "class." + clazz.getIdentifier().name;
    }

    public static String getStaticFieldName(FField field) {
        return "sf." + field.getMemberOf().getIdentifier().name + '.' + field.getIdentifier().name;
    }

    public static String getFunctionName(FFunction function) {
        String uniqueName = uniqueFunctionNameCache.get(function);
        if (uniqueName == null) {
            Map<FFunction, String> newNames = Utils.computeUniqueFunctionNames(((FClass) function.getMemberOf()).getFunctions());
            uniqueFunctionNameCache.putAll(newNames);
            uniqueName = newNames.get(function);
        }
        return function.isNative()
                ? function.getIdentifier().name
                : "fun." + function.getMemberOf().getIdentifier().name + '.' + uniqueName;
    }

    public static String getConstantStringName(String s) {
        return "const.String." + s;
    }

}
