package tys.frontier.tools;

import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.*;
import tys.frontier.backend.llvm.LLVMUtil;
import tys.frontier.style.Keywords;
import tys.frontier.util.Joiners;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.bytedeco.llvm.global.LLVM.*;

public class HeaderGenerator {

    private static final int TRUE = 1;
    private static final int FALSE = 0;

    private static final String IN = "imgui.bc";
    private static final String IN_INS = "imgui.ins";
    private static final String OUT = "imgui.front";


    public static void main(String[] args) throws IOException, URISyntaxException {
        Instructions instructions = Instructions.create(Path.of(IN_INS));

        LLVMModuleRef module = LLVMUtil.loadModuleFromBitcode(LLVMGetGlobalContext(), IN);
        LLVMContextRef context = LLVMGetModuleContext(module);

        ModuleInfo moduleInfo = parseModule(module, instructions);

        PrintStream out = new PrintStream(OUT); //System.out;
        out.println("/* Typemap:");
        o :for (Map.Entry<LLVMTypeRef, String> entry : moduleInfo.typeMap.entrySet()) {
            for (Pair<LLVMTypeRef, String> pair : moduleInfo.structs) {
                if (entry.getValue().equals(pair.b))
                    continue o; //TODO ridicilously inefficient, if this stays please make it better
            }
            out.println("   " + LLVMPrintTypeToString(entry.getKey()).getString() + " :: " + entry.getValue());
        }
        out.println("*/");

        out.println("\n//Hollow typedefs");
        for (Pair<LLVMTypeRef, String> struct : moduleInfo.structs) {
            String llvmName = LLVMGetStructName(struct.a).getString();
            if (struct.b == null)
                out.println("native class " + llvmName + ':');
            else
                out.println("native(\"" + llvmName + "\") class " + struct.b + ':');
        }

        out.println("\nnamespace Functions:");
        for (FileInfo file : moduleInfo.files.values()) {
            out.println("//File: " + metaDataToString(file.llvmRef, context));

            if (!file.noNamespace.functions.isEmpty()) {
                out.println("   //no Namespace:");
                for (FunctionInfo function : file.noNamespace.functions) {
                    //System.out.println("      //" + function.getLlvmDefinition());
                    out.println("      " + function.generateHeader(moduleInfo.typeMap));
                }
            }

            for (NamespaceInfo namespace : file.namespaces.values()) {
                out.println("   //Namespace: " + metaDataToString(namespace.llvmRef, context));
                for (FunctionInfo function : namespace.functions) {
                    //System.out.println("      //" + function.getLlvmDefinition());
                    out.println("      " + function.generateHeader(moduleInfo.typeMap));
                }
            }
        }
    }

    static class Instructions {
        List<String> ignoreNamespaces = new ArrayList<>();
        List<String> ignoreFiles = new ArrayList<>();

        static Instructions create(Path path) throws IOException {
            Instructions res = new Instructions();
            Files.lines(path).forEach(line -> {
                String[] ins = line.split(" ");
                assert ins.length == 3;
                assert ins[0].equals("d");
                List<String> target = switch (ins[1]) {
                    case "f" -> res.ignoreFiles;
                    case "ns" -> res.ignoreNamespaces;
                    default -> Utils.handleError("unknown token " + ins[1]);
                };
                target.add(ins[2]);
            });
            return res;
        }
    }

    static class ModuleInfo {
        LLVMContextRef context;
        Map<LLVMMetadataRef, FileInfo> files = new LinkedHashMap<>();
        Map<LLVMTypeRef, String> typeMap = new LinkedHashMap<>();
        List<Pair<LLVMTypeRef, String>> structs = new ArrayList<>();

        public ModuleInfo(LLVMContextRef context) {
            this.context = context;
        }

        void addFunction(FunctionInfo function, LLVMMetadataRef file, LLVMMetadataRef namespace) {
            FileInfo fileInfo = files.computeIfAbsent(file, FileInfo::new);
            if (namespace == null)
                fileInfo.noNamespace.functions.add(function);
            else
                fileInfo.namespaces.computeIfAbsent(namespace, NamespaceInfo::new).functions.add(function);
        }

        String addType(LLVMTypeRef llvmType) {
            String res = typeMap.get(llvmType);
            if (res == null) {
                res = createTypeMapping(llvmType);
                typeMap.put(llvmType, res);
            }
            return res;
        }

        String createTypeMapping(LLVMTypeRef llvmType) {
            switch (LLVMGetTypeKind(llvmType)) {
                case LLVMPointerTypeKind :
                    LLVMTypeRef baseType = LLVMGetElementType(llvmType);
                    switch (LLVMGetTypeKind(baseType)) {
                        case LLVMStructTypeKind:
                            String origName = LLVMGetStructName(baseType).getString();
                            String name = origName;
                            assert name.startsWith("struct.");
                            name = sanitize(name.substring(7));
                            if (!Character.isUpperCase(name.charAt(0)))
                                name = Character.toUpperCase(name.charAt(0)) + name.substring(1);

                            if (name.equals(origName))
                                structs.add(new Pair<>(baseType, null));
                            else
                                structs.add(new Pair<>(baseType, name));
                            return name;
                        case LLVMFunctionTypeKind:
                            int paramSize = LLVMCountParamTypes(baseType);
                            List<String> strings = new ArrayList<>(paramSize);
                            PointerPointer<LLVMTypeRef> params = new PointerPointer<>(paramSize);
                            LLVMGetParamTypes(baseType, params);
                            for (int i = 0; i < paramSize; i++) {
                                LLVMTypeRef param = params.get(LLVMTypeRef.class, i);
                                strings.add(addType(param));
                            }
                            String r = addType(LLVMGetReturnType(baseType));
                            return '(' + Joiners.ON_COMMA.join(strings) + " -> " + r + ')';
                        default:
                            return "native[" + addType(baseType) + ']';
                    }
                case LLVMIntegerTypeKind:
                    return switch (LLVMGetIntTypeWidth(llvmType)) {
                        case 1 -> "bool";
                        case 8 -> "char";
                        case 16, 32, 64 -> "int" + LLVMGetIntTypeWidth(llvmType);
                        default -> {
                            System.err.println("int of non standard size: " + LLVMGetIntTypeWidth(llvmType));
                            yield "int" + LLVMGetIntTypeWidth(llvmType);
                        }
                    };
                case LLVMFloatTypeKind:
                    return "float32";
                case LLVMDoubleTypeKind:
                    return "float64";
                case LLVMVoidTypeKind:
                    return "()";
                default:
                    System.err.println("unknown type: " + LLVMPrintTypeToString(llvmType).getString());
                    return "!!!!" + LLVMPrintTypeToString(llvmType).getString() + "!!!!";
            }
        }
    }

    static class FileInfo {
        LLVMMetadataRef llvmRef;
        NamespaceInfo noNamespace = new NamespaceInfo(null);
        Map<LLVMMetadataRef, NamespaceInfo> namespaces = new LinkedHashMap<>();

        public FileInfo(LLVMMetadataRef llvmRef) {
            this.llvmRef = llvmRef;
        }
    }

    static class NamespaceInfo {
        LLVMMetadataRef llvmRef;
        List<FunctionInfo> functions = new ArrayList<>();

        public NamespaceInfo(LLVMMetadataRef llvmRef) {
            this.llvmRef = llvmRef;
        }
    }


    static class FunctionInfo {
        LLVMValueRef llvmRef;
        LLVMTypeRef returnType;
        List<ParamInfo> params = new ArrayList<>();
        Map<String, String> attribs;

        String generateHeader(Map<LLVMTypeRef, String> typeMap) {
            return generateHeaderBeginning() + generateParams(typeMap) + ";";
        }

        String generateHeaderBeginning() {
            String name = attribs.get("name");
            String linkageName = attribs.get("linkageName");
            if (linkageName == null) {
                assert LLVMGetValueName(llvmRef).getString().equals(name);
                name = sanitize(name);
                if (Character.isLowerCase(name.charAt(0))) {
                    return "native " + name;
                } else {
                    return "native(\"" + name + "\") " + Character.toLowerCase(name.charAt(0)) + name.substring(1);
                }
            } else {
                assert LLVMGetValueName(llvmRef).getString().equals(linkageName);
                name = sanitize(name);
                if (!Character.isLowerCase(name.charAt(0)))
                    name = Character.toLowerCase(name.charAt(0)) + name.substring(1);
                return "native(\"" + linkageName + "\") " + name;
            }
        }

        String generateParams(Map<LLVMTypeRef, String> typeMap) {
            StringBuilder sb = new StringBuilder();
            sb.append('(');
            Joiners.ON_COMMA.appendTo(sb, Utils.map(params, p -> p.wupwup(typeMap)));
            sb.append(')');
            if (!returnType.equals(LLVMVoidType())) {
                sb.append(" -> ").append(typeMap.get(returnType));
            }
            return sb.toString();
        }

        String getLlvmDefinition() {
            String s = LLVMPrintValueToString(llvmRef).getString();
            int l1 = s.indexOf('\n');
            int l2 = s.indexOf('\n', l1+1);
            if (l2 < 0)
                return s;
            else
                return s.substring(l1+1, l2);
        }
    }

    static class ParamInfo {
        String name;
        LLVMTypeRef type;
        LLVMValueRef value;
        LLVMValueRef var;

        String wupwup(Map<LLVMTypeRef, String> typeMap) {
            return name + ": " + typeMap.get(type);
        }
    }

    private static ModuleInfo parseModule(LLVMModuleRef module, Instructions instructions) {
        ModuleInfo res = new ModuleInfo(LLVMGetModuleContext(module));

        for (LLVMValueRef function = LLVMGetFirstFunction(module); function != null; function = LLVMGetNextFunction(function)) {

            if (LLVMIsDeclaration(function) == TRUE)
                continue;

            int linkage = LLVMGetLinkage(function);
            switch (linkage) {
                case LLVMInternalLinkage:
                case LLVMPrivateLinkage:
                case LLVMLinkOnceAnyLinkage:
                case LLVMLinkOnceODRLinkage:
                    continue;
            }
            //int visibility = LLVMGetVisibility(function);

            parseFunction(function, instructions, res);
        }
        return res;
    }


    static int SRET = LLVMGetEnumAttributeKindForName("sret", 4);
    private static void parseFunction(LLVMValueRef function, Instructions instructions, ModuleInfo module) {
        FunctionInfo res = new FunctionInfo();
        res.llvmRef = function;

        LLVMMetadataRef subprogram = LLVMGetSubprogram(function);
        res.attribs = parseAttribs("DISubprogram", subprogram, module.context);

        LLVMMetadataRef file = LLVMDIScopeGetFile(subprogram);
        if (instructions.ignoreFiles.contains(parseAttribs("DIFile", file, module.context).get("filename")))
            return;

        LLVMMetadataRef scope = null;

        String scopeAttrib = res.attribs.get("scope");
        if (scopeAttrib != null) {
            scope = attribToMetadataPtr(scopeAttrib);
            if (scope.equals(file))
                scope = null;
            else {
                if (switch (LLVMGetMetadataKind(scope)) {
                    case LLVMDICompositeTypeMetadataKind ->
                        instructions.ignoreNamespaces.contains(parseAttribs("DICompositeType", scope, module.context).get("name"));
                    case LLVMDINamespaceMetadataKind ->
                            instructions.ignoreNamespaces.contains(parseAttribs("DINamespace", scope, module.context).get("name"));
                    default -> Utils.NYI("Function Scope of MetadataKind " + LLVMGetMetadataKind(scope) + ": " + metaDataToString(scope, module.context));
                }) return;
            }
        }

        res.returnType = LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function)));
        if (!res.returnType.equals(LLVMVoidTypeInContext(module.context)))
            module.addType(res.returnType);

        int paramsCount = LLVMCountParams(function);
        PointerPointer<LLVMValueRef> params = new PointerPointer<>(paramsCount);
        LLVMGetParams(function, params);
        for (int i = 0; i < paramsCount; i++) {
            LLVMValueRef param = params.get(LLVMValueRef.class, i);
            LLVMTypeRef type = LLVMTypeOf(param);
            ParamInfo info = new ParamInfo();
            info.type = type;
            info.value = param;
            info.name = "param" + i;
            LLVMAttributeRef llvmAttributeRef = LLVMGetEnumAttributeAtIndex(function, i+1, SRET);
            if (llvmAttributeRef != null && !llvmAttributeRef.isNull()) {
                info.name = "sret";
            }
            res.params.add(info);
            module.addType(type);
        }

        //see if we can find names for the parameters
        for (LLVMValueRef inst = LLVMGetFirstInstruction(LLVMGetEntryBasicBlock(function)); inst != null &&  !inst.isNull(); inst = LLVMGetNextInstruction(inst)) {
            //TODO check for @llvm.dbg.declare, if so check for first arg associated storage location, if so second arg is the metadata we want
            switch (LLVMGetInstructionOpcode(inst)) {
                case LLVMZExt -> {
                    //for bool params, they get a zext and stored in an i8
                    LLVMValueRef val = LLVMGetOperand(inst, 0);
                    for (ParamInfo param : res.params) {
                        if (param.value.equals(val)) {
                            param.value = inst;
                        }
                    }
                }
                case LLVMStore -> {
                    LLVMValueRef val = LLVMGetOperand(inst, 0);
                    for (ParamInfo param : res.params) {
                        if (param.value.equals(val)) {
                            param.var = LLVMGetOperand(inst, 1);
                        }
                    }
                }
                case LLVMCall -> {
                    LLVMValueRef func = LLVMGetCalledValue(inst);
                    if (LLVMGetValueName(func).getString().equals("llvm.dbg.declare")) {
                        for (int i = 0; i < res.params.size(); i++) {
                            ParamInfo param = res.params.get(i);

                            if (LLVMGetOperand(LLVMGetArgOperand(inst, 0), 0).equals(param.var)) {
                                LLVMMetadataRef paramMetadata = LLVMValueAsMetadata(LLVMGetArgOperand(inst, 1));
                                Map<String, String> paramAttribs = parseAttribs("DILocalVariable", paramMetadata, module.context);
                                param.name = sanitize(paramAttribs.get("name"));
                            }
                        }
                    }
                }
            }
        }

        module.addFunction(res, file, scope);
    }

    static Map<Character, Character> delimiters = Map.of('<', '>', '"', '"');

    static Map<String, String> parseAttribs(String type, LLVMMetadataRef metadata, LLVMContextRef context) {
        String metaDataString = metaDataToString(metadata, context);

        if (metaDataString.endsWith("!{}")) {
            return Map.of();
        }
        assert metaDataString.contains("= distinct !" + type + "(") ||
                metaDataString.contains("= !" + type + "(");
        metaDataString = metaDataString.substring(metaDataString.indexOf('(')+1, metaDataString.length()-1);

        Map<String, String> res = new LinkedHashMap<>();
        while (metaDataString != null) {
            metaDataString = readNextAttrib(metaDataString, res);
        }
        return res;
    }

    static String readNextAttrib(String s, Map<String, String> attribMap) {
        int i1 = s.indexOf(':');
        assert i1 >= 0;
        String key = s.substring(0, i1).trim();
        s = s.substring(i1 + 1).trim();
        if (delimiters.containsKey(s.charAt(0))) {
            int i2 = s.indexOf(delimiters.get(s.charAt(0)), 1);
            attribMap.put(key, s.substring(1, i2));
            if (s.length() == i2+1)
                return null;
            assert s.charAt(i2 + 1) == ',';
            return s.substring(i2 + 2);
        } else {
            int i2 = s.indexOf(',');
            if (i2 < 0) {
                attribMap.put(key, s.trim());
                return null;
            }
            attribMap.put(key, s.substring(0, i2).trim());
            return s.substring(i2 + 1);
        }
    }

    static LLVMMetadataRef attribToMetadataPtr(String attrib) {
        assert attrib.startsWith("0x");
        long l = Long.parseLong(attrib.substring(2), 16);
        return new LLVMMetadataRef(HackedPointer.of(l));
    }

    static String metaDataToString(LLVMMetadataRef metadata, LLVMContextRef context) {
        return LLVMPrintValueToString(LLVMMetadataAsValue(context, metadata)).getString();
    }

    static String sanitize(String name) {
        //TODO unsure if underscore or just remove is better... or maybe just keep no idea if we allow that tbh
        name = name.replaceAll("::", "_");
        name = name.replaceAll(":", "_");
        name = name.replaceAll("\\.", "_");
        name = name.replaceAll("~", "destroy");
        name = Utils.removeLeadingUnderscores(name);
        name = Utils.removeTrailingUnderscores(name);
        if (Keywords.DEFAULT_KEYWORDS.containsKey(name))
            return '_' + name;
        return name;
    }

    static class HackedPointer extends Pointer {
        public static HackedPointer of(long address) {
            HackedPointer res = new HackedPointer();
            res.address = address;
            return res;
        }
    }


}
