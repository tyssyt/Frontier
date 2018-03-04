package tys.frontier.backend.llvm;

import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;

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

}
