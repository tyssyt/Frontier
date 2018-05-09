package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import tys.frontier.code.FClass;
import tys.frontier.code.FField;
import tys.frontier.code.FFunction;

import java.util.List;

public class LLVMUtil {

    private LLVMUtil() {} //no instances

    public static <P extends Pointer> PointerPointer<P> createPointerPointer (List<P> list) {
        PointerPointer<P> res = new PointerPointer<>(list.size());
        for (int i=0; i<list.size(); i++)
            res.put(i, list.get(i));
        return res;
    }

    public static <P extends Pointer> PointerPointer<P> createPointerPointer (P first, List<P> rest) {
        int size = 1 + rest.size();
        PointerPointer<P> res = new PointerPointer<>(size);
        res.put(0, first);
        for (int i=1; i<size; i++)
            res.put(i, rest.get(i-1));
        return res;
    }

    public static String getClassName(FClass clazz) {
        return "class." + clazz.getIdentifier().name;
    }

    public static String getStaticFieldName(FField field) {
        return "sf." + field.getClazz().getIdentifier().name + '.' + field.getIdentifier().name;
    }

    public static String getFunctionName(FFunction function) {
        return "fun." + function.getClazz().getIdentifier().name + '.' + function.getIdentifier().name;
    }

}
