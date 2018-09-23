package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class LLVMUtil {

    private LLVMUtil() {} //no instances

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

    public static String getClassName(FClass clazz) {
        return "class." + clazz.getIdentifier().name;
    }

    public static String getStaticFieldName(FField field) {
        return "sf." + field.getMemberOf().getIdentifier().name + '.' + field.getIdentifier().name;
    }

    public static String getFunctionName(FFunction function) {
        return function.isNative()
                ? function.getIdentifier().name
                : "fun." + function.getMemberOf().getIdentifier().name + '.' + function.getMemberOf().getUniqueFunctionNames().get(function);
    }

    public static String getConstantStringName(String s) {
        return "const.String." + s;
    }

}
