package tys.frontier.parser.syntaxTree;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.TerminalNode;
import tys.frontier.code.FParameter;
import tys.frontier.code.FVisibilityModifier;
import tys.frontier.code.InstanceField;
import tys.frontier.code.StaticField;
import tys.frontier.code.function.FFunction;
import tys.frontier.code.function.FunctionBuilder;
import tys.frontier.code.function.NativeDecl;
import tys.frontier.code.function.operator.Operator;
import tys.frontier.code.identifier.FIdentifier;
import tys.frontier.code.namespace.DefaultNamespace;
import tys.frontier.code.namespace.Namespace;
import tys.frontier.code.statement.loop.forImpl.ForPlaceholder;
import tys.frontier.code.type.FClass;
import tys.frontier.code.type.FType;
import tys.frontier.code.type.FTypeVariable;
import tys.frontier.parser.Delegates;
import tys.frontier.parser.ParsedFile;
import tys.frontier.parser.antlr.FrontierBaseVisitor;
import tys.frontier.parser.antlr.FrontierParser;
import tys.frontier.parser.location.Location;
import tys.frontier.parser.location.Position;
import tys.frontier.parser.syntaxErrors.*;
import tys.frontier.util.Pair;
import tys.frontier.util.Utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class GlobalIdentifierCollector extends FrontierBaseVisitor<Object> {

    private ParsedFile file;
    private SyntaxTreeData treeData;
    private Delegates delegates;
    private List<SyntaxError> errors;

    private DefaultNamespace currentNamespace;
    private FClass currentClass;

    private GlobalIdentifierCollector(ParsedFile file, Delegates delegates, List<SyntaxError> errors) {
        this.file = file;
        this.treeData = file.getTreeData();
        this.delegates = delegates;
        this.errors = errors;
    }

    public static Delegates collectIdentifiers(ParsedFile file, Delegates delegates, List<SyntaxError> errors) {
        GlobalIdentifierCollector collector = new GlobalIdentifierCollector(file, delegates, errors);
        for (FrontierParser.ClassDeclarationContext ctx : collector.treeData.root.classDeclaration()) {
            ctx.accept(collector);
        }
        for (FrontierParser.NamespaceDeclarationContext ctx : collector.treeData.root.namespaceDeclaration()) {
            ctx.accept(collector);
        }
        return collector.delegates;
    }

    @Override
    public Object visitClassDeclaration(FrontierParser.ClassDeclarationContext ctx) {
        currentNamespace = treeData.classNamespaces.get(ctx);
        currentClass = currentNamespace.getType();
        try {
            visitChildren(ctx);
            return null;
        } finally {
            currentNamespace = null;
            currentClass = null;
        }
    }

    @Override
    public Object visitNamespaceDeclaration(FrontierParser.NamespaceDeclarationContext ctx) {
        currentNamespace = treeData.namespaces.get(ctx);
        try {
            visitChildren(ctx);
            return null;
        } finally {
            currentNamespace = null;
        }
    }

    @Override
    public Object visitConstructorsDeclarative(FrontierParser.ConstructorsDeclarativeContext ctx) {
        currentClass.setConstructorVisibility(ParserContextUtils.getVisibility(ctx.visibilityModifier()));
        return null;
    }

    @Override
    public Object visitForDeclarative(FrontierParser.ForDeclarativeContext ctx) {
        currentClass.setForImpl(ForPlaceholder.INSTANCE);
        return null;
    }

    @Override
    public Object visitMethodDeclaration(FrontierParser.MethodDeclarationContext ctx) {
        return visitMethodHeader(ctx.methodHeader(), true);
    }

    @Override
    public Object visitNativeMethodDeclaration(FrontierParser.NativeMethodDeclarationContext ctx) {
        return visitMethodHeader(ctx.methodHeader(), false);
    }

    public Object visitMethodHeader(FrontierParser.MethodHeaderContext ctx, boolean hasBody) {
        FunctionBuilder builder = new FunctionBuilder();

        //type Parameters
        Function<FIdentifier, Namespace> typeResolver;
        {
            FrontierParser.TypeParametersContext c = ctx.typeParameters();
            if (c != null) {
                try {
                    Map<FIdentifier, FTypeVariable> typeParameters = Utils.asTypeMap(ParserContextUtils.getTypeParameters(c).a);
                    if (currentClass != null)
                        for (FType classParam : currentClass.getParametersList())
                            if (typeParameters.containsKey(classParam.getIdentifier()))
                                throw new TwiceDefinedLocalVariable(classParam.getIdentifier());
                    typeResolver = id -> Optional.ofNullable(typeParameters.get(id)).map(FTypeVariable::getNamespace).orElseGet(() -> resolveNamespace(id));
                    builder.setParameters(typeParameters);
                } catch (TwiceDefinedLocalVariable twiceDefinedLocalVariable) {
                    errors.add(twiceDefinedLocalVariable);
                    return null;
                }
            } else {
                typeResolver = this::resolveNamespace;
            }
        }

        //return type
        {
            FrontierParser.TypeListContext c = ctx.typeList();
            if (c != null) {
                try {
                    builder.setReturnType(ParserContextUtils.tupleFromList(c, typeResolver));
                } catch (SyntaxError e) {
                    errors.add(e);
                }
            }
        }

        //assignees
        {
            ImmutableList.Builder<FParameter> b = ImmutableList.builder();
            FrontierParser.TypedIdentifiersContext ct = ctx.typedIdentifiers();
            if (ct != null) {
                for (FrontierParser.TypedIdentifierContext c : ct.typedIdentifier()) {
                    try {
                        Pair<FIdentifier, FType> pair = ParserContextUtils.getTypedIdentifier(c, typeResolver);
                        b.add(FParameter.create(pair.a, pair.b, false));
                    } catch (SyntaxError e) {
                        errors.add(e);
                    }
                }
                builder.setAssignees(b.build());
            }
        }

        ImmutableList.Builder<FParameter> params = ImmutableList.builder();
        if (currentClass != null && ctx.STATIC() == null) {
            params.add(FParameter.create(FIdentifier.THIS, currentClass, false));
        }


        try {
            formalParameters(ctx.formalParameters(), params, typeResolver);
            ImmutableList<FParameter> parameters = params.build();

            //identifier
            DefaultNamespace namespace;
            TerminalNode identifierNode = ctx.IDENTIFIER();
            if (identifierNode != null) {
                builder.setIdentifier(new FIdentifier(identifierNode.getText()));
                FrontierParser.TypeTypeContext typeTypeContext = ctx.typeType();
                if (typeTypeContext != null)
                    namespace = (DefaultNamespace) ParserContextUtils.getNamespace(typeTypeContext, typeResolver);
                else
                    namespace = currentNamespace;
            } else {
                //Operator overloading
                if (currentClass == null)
                    return Utils.NYI("operator overloading in namespace without class");
                Operator operator = Operator.get(ctx.operator().getText(), Utils.typesFromExpressionList(parameters));
                if (!operator.isUserDefinable())
                    return Utils.NYI("non overridable Operator aka FunctionNotFoundOrSth"); //TODO
                builder.setIdentifier(operator.getIdentifier());
                namespace = operator.getNamespace().orElse(currentNamespace);
            }

            builder.setVisibility(ParserContextUtils.getVisibility(ctx.visibilityModifier()));
            NativeDecl nativeDecl = ParserContextUtils.getNative(ctx.nativeModifier());
            if (nativeDecl != null)
                builder.setNative(nativeDecl);
            boolean open = ctx.OPEN() != null;

            Location location = new Location(currentNamespace.getLocation().getFile(), Position.fromCtx(ctx));
            FFunction res = builder.setLocation(location).setMemberOf(namespace).setParams(parameters).build();
            treeData.functions.put(ctx, res);

            if (nativeDecl != null && hasBody)
                throw new NativeWithBody(res);

            if(!open || nativeDecl != null || hasBody) //open non native functions without body should not be added
                namespace.addFunction(res);

            if (open) //mark as open
                namespace.setOpen(res);

            if (namespace != currentNamespace) //mark as remote
                currentNamespace.addRemoteFunction(res);

        } catch (SyntaxErrors e) {
            errors.addAll(e.errors);
        } catch (SyntaxError e) {
            errors.add(e);
        }
        return null;
    }

    public void formalParameters(FrontierParser.FormalParametersContext ctx, ImmutableList.Builder<FParameter> params, Function<FIdentifier, Namespace> possibleNamespaces) throws SyntaxErrors {
        List<FrontierParser.FormalParameterContext> cs = ctx.formalParameter();
        List<SyntaxError> errors = new ArrayList<>();
        for (FrontierParser.FormalParameterContext c : cs) {
            try {
                FParameter param = ParserContextUtils.getParameter(c, possibleNamespaces);
                treeData.parameters.put(c, param);
                params.add(param);
            } catch (SyntaxError e) {
                errors.add(e);
            }
        }
        if (!errors.isEmpty())
            throw SyntaxErrors.create(errors);
    }

    @Override
    public Object visitFieldDeclaration(FrontierParser.FieldDeclarationContext ctx) {
        FVisibilityModifier visibilityModifier = ParserContextUtils.getVisibility(ctx.visibilityModifier());
        boolean _static = currentClass == null || ParserContextUtils.isStatic(ctx.modifier());
        try {
            FIdentifier identifier = new FIdentifier(ctx.IDENTIFIER().getText());
            FType type = ParserContextUtils.getType(ctx.typeType(), this::resolveNamespace);
            if (_static) {
                StaticField res = new StaticField(Position.fromCtx(ctx), identifier, type, currentNamespace, visibilityModifier, ctx.expression() != null);
                if (ctx.nameSelector() != null)
                    throw new DelegateFromStaticField(res);
                currentNamespace.addField(res);
                treeData.fields.put(ctx, res);
            } else {
                InstanceField res = new InstanceField(Position.fromCtx(ctx), identifier, type, currentClass, visibilityModifier, ctx.expression() != null);
                currentClass.addField(res);
                treeData.fields.put(ctx, res);

                FrontierParser.NameSelectorContext c = ctx.nameSelector();
                if (c != null) {
                    if (c.STAR() != null && c.BACKSLASH() == null)
                        currentClass.addDelegate(res);
                    delegates.add(res, ParserContextUtils.getNameSelector(c));
                }
            }
        } catch (SyntaxError e) {
            errors.add(e);
        }
        return null;
    }

    private Namespace resolveNamespace(FIdentifier identifier) {
        if (currentClass != null)
            for (FType p : currentClass.getParametersList())
                if (p.getIdentifier().equals(identifier))
                    return p.getNamespace();
        return file.resolveNamespace(identifier);
    }
}
